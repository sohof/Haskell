module Evol where

class Evol a where 
  distance :: a -> a -> Float
  myLength :: a -> Int
  name :: a -> String
  distanceMatrix :: [a] -> [[(String,String,Float)]]

  distanceMatrix list = [[(name y,name x, avstand) | x <- list, let avstand=distance x y] | y <-list ]
