module ProfSeq where
import Data.List
import MolSeq
import Evol

type Matrix = [[(Char,Float)]]
nucleotides = "ACGT"
aminoacids = sort "ARNDCEQGHILKMFPSTWYVX"

instance Evol Profile where
  distance prof1 prof2 = profileDistance prof1 prof2
  myLength profile = nrOfSeqs(profile)  
  name profile = profName profile


-- Datatype for Profiles of molecular sequences.Holds: Name of profile, 
-- profile Type (DNA/Protein), nr of Sequences the profile is build on
-- and the actual profile itself in form of a matrix
data Profile = Profile { profName :: String, profType :: String, nrOfSeqs :: Int, profMatrix  :: Matrix
                       } deriving (Show,Read,Eq)
                                  
  
fromMolSeqs :: [MolSeq] -> Profile
fromMolSeqs [] = error "Empty Molecule list"
fromMolSeqs molSeqs = Profile profName profType nrOfSeqs profMatrix
                                where
                                  profName = seqName(head(molSeqs)) --Just give name of first molseq 
                                  profType = seqType(head(molSeqs)) -- all molseqs should have same type
                                  nrOfSeqs = length molSeqs      -- nrOF molecules seqs,profile is based on
                                  profMatrix =  makeProfileMatrix molSeqs -- function to create the matrix
  
makeProfileMatrix :: [MolSeq] -> Matrix
makeProfileMatrix [] = error "Empty sequence list"
makeProfileMatrix sl = res
  where 
    t = seqType (head sl)
    n = fromIntegral(length sl)
    defaults = if (t == "DNA") then
                 zip nucleotides (replicate (length nucleotides) 0) -- Rad (i)
               else 
                 zip aminoacids (replicate (length aminoacids) 0)   -- Rad (ii)
    strs = map seqSequence sl                                       -- Rad (iii)
    tmp1 = map (map (\x -> ((head x), fromIntegral(length x)/n)) . group . sort)  -- Rad (iv)
               (transpose strs)
    equalFst a b = (fst a ) == (fst b)                                --Rad (v)
    res = map sort (map (\l -> unionBy equalFst l defaults) tmp1)  --Rad (vi) 


-- Rad i&ii: 
-- Here we check the type of molecule we are working with. Then we create
-- a list of zeros to zip with the molecule type. Creating a list of pairs for e.g [('A',0),('B',0) = defaults
    
-- Rad iii: 
-- Here we map the seqSequence functions on the list to extract all sequences and save them in the 
-- strs list as "row vectors". That is how we can choose to view it! E.g  STRS=[[ATC],[CGA],[AAG]]                                
    
-- Rad iv: 
-- Here First we transpose our "strs" matrix so our row vectors turn into "column vectors" this way 
-- we collect call all our position X elements from our sequences into one list/row vector. So all chars
-- in index/position 1 from each seqeuence end up in the same vector/list for further analysis. E.g
-- now we have transpose STRS=[[ACA],[TGA],[CAG]]. [TGA] are all chars from index 2 in each sequence.    
-- now for each element in this list we map the function (map (\x -> ((head x), (length x))).group.sort)    
-- which takes each index vector and first sorts it: [ACA] -> [A,A,C], then applies the group function 
-- which takes a list and groups adjacent elements into sublists if they are equal. Thus [A,A,C]->
-- [[A,A],[C]]. To each element in this we apply (\x -> ((head x), fromIntegral(length x)/n)). Which basically
-- counts the occurences of each amino acid/nucleotied represented by a Char and creates a tuple    
-- "Char,Count", we now know the nr of occurences of each amino acid in a given position/index vector.
-- The count for each given Char is then divided by the nr of sequences, to give is the frequency of 
-- how often it occurs in relation to the nr of sequences we are working with.     
-- The outer map applies this operation to each of our index/position vector. So our tmp1 is now
-- holding a list of each of our index/position vectors, where each of these vectors hold a list  
-- containing tuples where we have info on the occurence of each amino acid in that vector.    
-- Now our tmp1 matrix is starting to look like the C-matrix. The C-matrix where each column X represents 
-- all chars in index X, coming from all sequences and has a count for the nr of times the given Char/nucleotides 
-- appear in that position position vector. The difference is that e.g vector for position one, does contain a 
-- count of "zero" for Chars not appearing in that vector. For example from our vector [[A,A],[C]] will get
-- [('A',2),('C',1)], but we have no index and count zero for the missing nucleotides 'T' and 'G'. 
    
-- Rad v: 
-- To create the C-Matrix, we now create the predicate equalFst to be used be the unionBy
-- function for lists.   
    
-- Rad vi:      
-- Now we map the function (\l -> unionBy equalFst l defaults) on our tmp1. Which takes each
-- column l from tmp1 and uses unionBy on it with the list of all nucleotiedes/amino acids stored
-- in the list defaults. Thus it complements/extends our matrix  to make sure we in all columns 
-- also keep track of all occuring Chars even though the count in the given column is equal to zero! 
-- Now just to have all columns appearing in alphabetic order we map sort over it.     
-- Matrix  M is created from matrix C by dividing each element count with the total nr of seqences 


profileDistance :: Profile -> Profile -> Float
profileDistance (Profile _ _ _ profmatrix_1) (Profile _ _ _ profmatrix_2)  = calcDistance profmatrix_1 profmatrix_2
                                 
                                                                           
calcDistance :: Matrix -> Matrix -> Float
calcDistance [] _ = 0
calcDistance _ [] = 0  -- We calc elementwise difference for one pair of columns at a time. 
calcDistance (headpm1:tail1) (headpm2:tail2) = (sumDifference headpm1 headpm2) + calcDistance tail1 tail2 
  where 
    sumDifference [] _ = 0 -- Here we go through each tuple in each columnt from the 2 matrices
    sumDifference _ [] = 0 -- and calc. abs of their difference. 
    sumDifference (tuple1:rest1) (tuple2:rest2) = abs(snd(tuple1)-snd(tuple2))+ sumDifference rest1 rest2
          