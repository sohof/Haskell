{- Haskell Lab F4: In- och utmaning i Haskell:
Den här uppgiften behandlar in- och ut-matning i Haskell och utformning av ett fullständigt 
program i Haskell. Du ska anpassa ditt tidigare arbete, lab F2 eller F3, så att indata tas från 
fil istället för en lista av strängar eller en datastruktur definierad i programmet. -}

import System.Environment(getArgs)     
import Neighbor(neighbor)          -- neighbor :: DistanceMatrix -> String. 
import MolSeq(string2seq)          -- string2seq :: String -> String -> MolSeq
import Evol(distanceMatrix)        -- distanceMatrix :: [a] -> [[(String,String,Float)]]
import Data.List  

main = do        
   args <- getArgs  
   processArg args
   
processArg :: [String] -> IO ()
processArg args | length args <= 0 = do
                                      putStrLn ("Processing input from Standard In. Output to Standard Out")
                                      contents <-getContents 
                                      putStrLn (processInput contents)
                | length args == 1 = do
                                     let infilePath = args !! 0 
                                     putStrLn ("Processing " ++ infilePath ++ " Output to Standard Out") 
                                     contents <-readFile infilePath                 
                                     putStrLn $ processInput contents
                | length args == 2 = do
                                     let infilePath  = args !! 0 
                                         outfilePath = args !! 1 
                                     putStrLn ("Processing " ++ infilePath ++ " Output Saved in " ++ outfilePath)                     
                                     contents <-readFile infilePath                 
                                     writeFile outfilePath $ processInput contents
                | otherwise        = putStrLn "To many arguments"


processInput :: String -> String
processInput contents = let (ids,seqs) = partition (elem '>') $ lines contents 
                            tuples     = zip (map (takeWhile (/= ' ')) (map (drop 1) ids)) seqs 
                            dM         = distanceMatrix $ map (\(id,seq) -> string2seq id seq) tuples
                        in (neighbor dM)

{- partition creates 2 separate lists of the ids and seqs. Then we clean the ids from '>' and then take chars only to 
the first bland ' '. Then zip the cleaned version of ids with seqs to create a tuple of lists. We need the input in tuple 
form to map string2seq over them which gives us a list of [Molseqs]. Which is later used as input to distanceMatrix. 
Then call neihbor on the distanceMatrix to gives us a newick tree represenation in form a string. -}