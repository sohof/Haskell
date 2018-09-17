module NJ (neighbor) where

import Data.List
import Data.Char
import Data.Maybe
import qualified Data.Map as Map
import qualified Data.Set as Set


sort' x y 
	| snd(x) < snd(y) = LT
	| snd(x) > snd(y) = GT
	| otherwise = EQ

--(a,b,c) -> ((a,b),c) gör om en triple till tuple
fromTriple :: [(String,String,Float)] -> Map.Map (String,String) Float
fromTriple d = Map.fromList (map (\(x,y,z) -> ((x,y),z)) d)

	
--exempel matris vi får från MolSeq
test1 :: [(String, String, Float)]
test1 = [("a","a",0.0),("a","b",2.0),("a","c",3.0),("a","d",5.0),("a","e",6.0),("a","f",5.0),("b","a",2.0),("b","b",0.0),("b","c",3.0),("b","d",5.0),("b","e",6.0),("b","f",5.0),("c","a",3.0),("c","b",3.0),("c","c",0.0),("c","d",4.0),("c","e",5.0),("c","f",4.0),("d","a",5.0),("d","b",5.0),("d","c",4.0),("d","d",0.0),("d","e",5.0),("d","f",4.0),("e","a",6.0),("e","b",6.0),("e","c",5.0),("e","d",5.0),("e","e",0.0),("e","f",3.0),("f","a",5.0),("f","b",5.0),("f","c",4.0),("f","d",4.0),("f","e",3.0),("f","f",0.0)]

--testfall
test2 :: [(String, String, Float)]
test2 = [("a","a",0.0),("a","b",9.0),("a","c",3.0),("a","d",5.0),("b","a",9.0),("b","b",0.0),("b","c",3.0),("b","d",9.0),("c","a",3.0),("c","b",3.0),("c","c",0.0),("c","d",4.0),("d","a",5.0),("d","b",9.0),("d","c",4.0),("d","d",0.0)]

test3 :: [(String, String, Float)]
test3 = [("a","a",0.0),("a","b",2.0),("a","c",3.0),("b","a",2.0),("b","b",0.0),("b","c",3.0),("c","a",3.0),("c","b",3.0),("c","c",0.0)]


--stödfunktioner till tree
--tar ut de värden en förgrening har
func :: Map.Map String String -> String -> [String]
func map x = Map.keys (Map.filter (== x) map) 

--delar upp tupeln vi får från nj4 och kallar på tree som ger varje delträd
treefunc :: (Set.Set [Char], Map.Map String String) -> String
treefunc (set, map) = "("++(tree (s!!0) map)++","++(tree (s!!1) map) ++","++ (tree (s!!2) map) ++")" where s = Set.toList set

 
--bygger upp en sträng, som representerar ett delträd
tree :: String -> Map.Map String String -> String 
tree s m
	| s == [] = ""
	| 'v' == (head s) = "("++(tree ((func m s)!!0) m)++","++(tree ((func m s)!!1) m)++")"
	| otherwise = s


neighbor :: [(String,String,Float)] -> String
neighbor d1 = 
	let 
		f1 = Set.fromList (map (\(x,_,_) -> x) d1)
		e = Map.empty
		i = '1' 
		d = fromTriple d1 
	in treefunc (nj4 e i d f1)

--funktion som loopar igenom steg 4 i neighbor joining tills f är endast 3 element
nj4 :: Map.Map String String -> Char -> Map.Map (String,String) Float -> Set.Set [Char] -> (Set.Set [Char], Map.Map String String)
nj4 e i d f 
	| Set.size f == 3 = (f, e)
	| otherwise = nj4 ei ii di fi
	where 
		min = minS d f
		fi = Set.delete (fst min) (Set.delete (snd min) (Set.insert ('v':i:[]) f))
		ei = Map.insert (snd min) ('v':i:[]) (Map.insert (fst min) ('v':i:[]) e)
		di = makeD d min i (Set.delete (fst min) (Set.delete (snd min) f)) 
		ii = chr((ord i) +1)

		
--skapar Di+1 genom union på en Map med all element som innehåller Vi och en Map med alla element som inte innehåller a eller b, och insertar ((Vi,Vi),0.0) 
makeD d min i nodeList = Map.insert (('v':i:[]),('v':i:[])) 0.0 (Map.union (createViElements d min i nodeList) (Map.filterWithKey (\k _ -> elem k [(x,y)|x<-Set.toList nodeList,y<-Set.toList nodeList]) d)) 

--createV skapar en map med av avstånd från nya hörnparet till alla andra hörn
createViElements :: Map.Map (String,String) Float -> (String,String) -> Char -> Set.Set [Char]-> Map.Map (String,String) Float
createViElements d min i nodeList = Set.foldl (\acc x -> let dist = (((d Map.! (x,(fst min))) + (d Map.! (x,(snd min)))) /2) in Map.insert (('v':i:[]),x) dist (Map.insert (x,('v':i:[])) dist acc)) Map.empty nodeList


--returnerar de två hörn som ligger längst ifrån de andra (i en tripel)
minS :: Map.Map (String,String) Float -> Set.Set [Char] -> (String, String)
minS d f = fst((sortBy sort' [((x, y), dist) | (x,y) <- Map.keys d, let dist = (urvalsfunkS x y d f), x/=y]) !! 0)

--urvalsfunktion som räknar ut hur långt ett par hörn är från de andra hörnen
urvalsfunkS :: String -> String -> Map.Map (String,String) Float -> Set.Set [Char] -> Float
urvalsfunkS x y d f = ((fromIntegral (Set.size f)) - 2)*(d Map.! (x,y)) - (distanceSum x y d f)

--stödfunktion till s, som beräknar summan av avståndet mellan hörnen i paret till alla de andra hörnen
distanceSum :: String -> String -> Map.Map (String,String) Float -> Set.Set [Char] -> Float
distanceSum x y d f = Set.foldl (\acc z -> acc+((d Map.! (x,z)) + (d Map.! (y,z)))) 0 f

