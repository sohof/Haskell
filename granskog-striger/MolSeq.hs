module MolSeq where

import Data.List

data MolSeq = MolSeq {seqType::String, seqName::String, seqSequence::String} deriving (Show)
data Profile = Profile {typ::String, name::String, nr::Int, matrix::[[(Char,Float)]]} deriving (Show)

class Evol a where 
	distance :: a -> a -> Float
	name' :: a -> String
	distanceMatrix :: [a] -> [(String,String,Float)]
	distanceMatrix list = [(name1,name2,d)| x<-list, y<-list, let d = distance x y, let name1 = name' x, let name2 = name' y]

instance Evol MolSeq where
	distance a b = seqDistance a b
	name' a = seqName a

instance Evol Profile where
	distance a b = profileDistance a b
	name' a = name a

seqDistance :: MolSeq -> MolSeq -> Float
seqDistance (MolSeq seqType1 _ seq1) (MolSeq seqType2 _ seq2)
	| seqType1 == "DNA" && seqType2 == "DNA" = seqDistanceDNA seq1 seq2 (max (length seq1) (length seq2))  0
	| seqType1 == "Protein" && seqType2 == "Protein" = seqDistanceProtein seq1 seq2 (max(length seq1) (length seq2)) 0
	| otherwise = error("Kan inte jämföra DNA med Protein!")

seqDistanceDNA:: String -> String -> Int -> Int -> Float
seqDistanceDNA seq1 seq2 k a
	| seq1 == [] || seq2 == [] = distanceDNA ((fromIntegral a + fromIntegral (abs(length seq1 - length seq2)))/ fromIntegral k)
	| head seq1 == head seq2 = seqDistanceDNA (tail seq1) (tail seq2) k a
	| otherwise =  seqDistanceDNA (tail seq1) (tail seq2) k (a+1)
	
seqDistanceProtein:: String -> String -> Int -> Int -> Float
seqDistanceProtein seq1 seq2 k a
	| seq1 == [] || seq2 == [] = distanceProtein ((fromIntegral a + fromIntegral (abs(length seq1 - length seq2)))/ fromIntegral k)
	| head seq1 == head seq2 = seqDistanceProtein (tail seq1) (tail seq2) k a
	| otherwise =  seqDistanceProtein (tail seq1) (tail seq2) k (a+1)

distanceDNA:: Float -> Float
distanceDNA a 
	| a > 0.74 = 3.3
	| otherwise = (-3/4)*log(1-(4*a/3))

distanceProtein:: Float -> Float
distanceProtein a 
	| a > 0.94 = 3.7
	| otherwise = (-19/20)*log(1-(20*a/19))


profileDistance :: Profile -> Profile -> Float
profileDistance p1 p2 = profileDistance' (matrix p1) (matrix p2) 0

profileDistance' :: [[(Char,Float)]] -> [[(Char,Float)]] -> Float -> Float
profileDistance' matrix1 matrix2 d
	| matrix1 == [] || matrix2 == [] = d
	| otherwise = profileDistance' (tail matrix1) (tail matrix2) (d + columnDistance (head matrix1) (head matrix2) 0)


columnDistance :: [(Char, Float)] -> [(Char,Float)] -> Float -> Float
columnDistance list1 list2 d
	| list1 == [] || list2 == [] = d
	| otherwise = columnDistance (tail list1) (tail list2) (d + abs(snd(head list1) - snd(head list2)))


string2seq :: String -> String -> MolSeq
string2seq name seq = string2seq' name seq seq
string2seq' name seq whole
	| seq == [] = MolSeq "DNA" name whole
	| not (isDNA (head seq)) = MolSeq "Protein" name whole
	| otherwise = string2seq' name (tail seq) whole

isDNA c = 
	let s = "ACGT"
	in elem c s


seqLength :: MolSeq -> Int
seqLength mol = length (seqSequence mol)

nucleotides = "ACGT"
aminoacids = sort "ARNDCEQGHILKMFPSTWYVX"

makeProfileMatrix :: [MolSeq] -> [[(Char,Int)]]
makeProfileMatrix [] = error "Empty sequence list"
makeProfileMatrix sl = res --sl är lista med MolSeq
  where 
    t = seqType (head sl) --typen av första MolSeq
    n = length sl  --antal MolSeq i sl
    defaults = if (t == "DNA") then
				 --zip parar ihop två listor -> [('A',0)..('T',0)]
                 zip nucleotides (replicate (length nucleotides) 0) -- Rad (i)
               else 
                 zip aminoacids (replicate (length aminoacids) 0)   -- Rad (ii)
	--lista med alla sekvenser
    strs = map seqSequence sl                                       -- Rad (iii)
	--skapar listor med tuples, som visar hur ofta en bokstav förekommer på den platsen
    tmp1 = map (map (\x -> ((head x), (length x))) . group . sort)  -- Rad (iv)
               (transpose strs)
	--ger true om första i listan a == första i listan b
    equalFst a b = (fst a ) == (fst b)
	--skapar matrisen genom att jämföra första i tuplerna (bokstäverna); om det finns i tmp1 lägg ej till, annars lägg till från defaults
	--och sedan sorterar så att det alltid är i bokstavsordning för varje plats
    res = map sort (map (\l -> unionBy equalFst l defaults) tmp1)

fromMolSeqs :: [MolSeq] -> Profile
fromMolSeqs sl = Profile (seqType (head sl)) (seqName (head sl)) (length sl) (fromMatrix sl)

fromMatrix :: [MolSeq] -> [[(Char, Float)]]
fromMatrix sl = map(map(\x -> (fst(x),((fromIntegral $ snd(x))/(fromIntegral (length sl)))))) (makeProfileMatrix sl)






