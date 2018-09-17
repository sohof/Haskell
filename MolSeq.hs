module MolSeq where
import Data.List
import Debug.Trace
import Evol
instance Evol MolSeq where
  distance mol1 mol2 = seqDistance mol1 mol2 
  myLength mol = seqLength(mol)  
  name mol = seqName mol
  
data MolSeq = MolSeq {seqName::String, seqSequence :: String, seqType :: String} 
            deriving (Eq, Show, Read)
                                                  
-- By Using record syntax as above, haskell auotamtically defines the functions
-- seqName, seqSequence, secType for us. 

string2seq :: String -> String -> MolSeq
string2seq n s |isDNA s = MolSeq {seqName=n, seqSequence=s, seqType = "DNA"}
               |otherwise = MolSeq {seqName=n, seqSequence=s, seqType = "PROTEIN"}
              
-- Param n,s both Strings, stand for name and sequence. We create a Molseg depending what
-- type of characters we find in the string s. A string is DNA if it is the empty string, 
-- (we need to handle the [] case somehow) or it is DNA if the head is an element in "ACGT" 
-- and the Tail is DNA. If the string contains any other chars beside A,C,G,T then it is a PROTEIN                        
                     
isDNA :: String -> Bool                      
isDNA [] = True
isDNA (x:xs) = elem x "ACGT" && isDNA xs
-- Funcion takes a string and checks if all chars consists of either A,C,G or T. 

seqLength ::  MolSeq -> Int
seqLength molecule = length (seqSequence molecule)

seqNamev2 :: MolSeq -> String
seqNamev2 (MolSeq name _ _ ) = name -- This function exits only in case they complain on the use of record syntax!
  
    
seqDistance ::  MolSeq -> MolSeq -> Float
seqDistance mol1 mol2 
  |comparingDNAS      = if alpha > 0.74 then 3.3 else (-0.75*log(1-(4*alpha/3)))
  |comparingPROTEINS  = if alpha > 0.94 then 3.7 else (-0.95*log(1-(20*alpha/19)))
  |otherwise = error "comparing a protein molecule to a DNA molecule"
  where comparingDNAS = (seqType mol1 == "DNA") && (seqType mol2 =="DNA")  
        comparingPROTEINS = (seqType mol1 == "PROTEIN") && (seqType mol2=="PROTEIN") 
        seq1 = seqSequence mol1
        seq2 = seqSequence mol2
        alpha = (sum [1| (x,y)<- (zip seq1 seq2), x/=y]) /fromIntegral (seqLength mol1)
        
-- First check if both sequences are DNA. Second check if both are proteins.        
-- Do right type of calc. depending on molecule type. Give error if comparing different types!
-- For calc.alpha first Zip the two sequences. Then compare the pairs in the resulting list. 
-- For each char/position that they differ add one to the list, Sum the list & Divide with molecule length 