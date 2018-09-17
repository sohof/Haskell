module LabF1 where
import Data.Char

--With pattern matching and usual recursion. Dubblrekursion växer exponentielt.
fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib(n-2)


ackfib :: Int -> Int

ackfib 0 = 0
ackfib 1 = 1
ackfib n = fibAux n 1 0

--Using guards to check for n<0. When tested neg.param must be enclosed in (). Växer linjärt.
fibAux n f1 f0 | n<0  = error "n must be > 0" 
               | n==0 = f0
               | otherwise = fibAux (n-1) (f1+f0) f1

rovarsprak :: [Char] -> [Char]
rovarsprak [] = []
rovarsprak (x:xs) |isVokal x =  x:rovarsprak xs
                  |otherwise = x:'o':x:rovarsprak xs              

isVowel :: Char -> Bool
isVowel x = elem x "aeiouyåäö "
isVokal :: Char -> Bool
isVokal x  | x == 'a' = True
           | x == 'e' = True
           | x == 'i' = True         
           | x == 'o' = True
           | x == 'u' = True
           | x == 'y' = True
           | x == 'å' = True
           | x == 'ä' = True
           | x == 'ö' = True
           | x == ' ' = True
           | otherwise = False             
                         
svenska :: [Char] -> [Char]
svenska [] = []
svenska (x:xs) |isVokal x = x:(svenska xs)
               |otherwise = x: svenska (drop 2 xs) 


--a is of type Ord class, merge takes 2 sorted lists of type a and returns one 
-- list of type a, the merged version of the two lists. 

merge :: Ord a => [a] -> [a] -> [a]
merge [] [] = [] -- Denna basfall behövs egentligen inte. Vi når aldrig hit! 
merge xs [] = xs
merge [] ys = ys
merge fst@(x:xs) snd@(y:ys)     | y>= x      =  x : merge xs snd
                                | otherwise  =  y : merge fst ys

--merge a (x:restOfSecondList)	| x>= head a = [head a] ++ merge (drop 1 a) ([x] ++ restOfSecondList)
-- Sämre version ?? 	       	| otherwise  = [x] ++ merge a restOfSecondList



-- Två extra parameterar för att hålla reda på längden av det ord vi tittar på just nu "local" samt 
-- global för att hålla reda på det längsta ordet i strängen. Om c isAlpha plussa på local 
-- counter, anropa loop med resten av ordet. Om c ej är alpha har vi stött på ' ', nu ifall 
-- local är större än vår global max så skicka in local som vår "nya" global annars 
-- nollställer vi local och fortsätter bearbeta strängen.
maxOrd :: String -> Int
maxOrd s = loop 0 0 s
    where 
      loop global local [] = if local>global then local else global
      loop global local (c:str)
        | isAlpha c    = loop global (local+1) str 
        | global<local = loop local 0 str      
        | otherwise    = loop global 0 str 

--Kan istället ha loop max (global local) 0 str i otherwise fallet,
--så kan vi skippa mellan raden global<local = loop ...
                         
str5 = "\99a\t456"
