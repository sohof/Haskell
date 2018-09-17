module Neighbor  (neighbor) where -- The module only exports "neighbor"
import Data.Maybe 
import Data.List  
import qualified Data.Set as Set  
import qualified Data.Map as Map 
import Molbio
import Evol

-- Added types for readability
type Node = String
type Edge = (String,String)
type NodePair = (String,String)
type DistanceMatrix = [[(String,String,Float)]]  -- See DOCUMENTATION SECTION (1)
type DistanceMap = Map.Map NodePair Float
type EdgeMap  = Map.Map Node Edge 
type Vertices = Set.Set Node

-- Use output from neighborJoining to create string in Newick-format
neighbor :: DistanceMatrix -> String
neighbor dm = newick (neighborJoining dm)
        
-- neighborJoining takes a matrix and gives us a tree T=(V,E)
neighborJoining ::DistanceMatrix -> (Vertices, EdgeMap)    
neighborJoining matris = loopNJ fSet dMap fSet eMap 1
                         where
                           fSet = createVerticeSet matris   -- initially vSet = fSet 
                           dMap = getDistanceMap matris
                           eMap = Map.empty
                           
loopNJ fSet dMap vSet eMap count  
  | Set.size fSet > 3 =  let  
       (x,y) = selectMinNodePair fSet dMap
       fSet_tmp1 = Set.difference fSet (Set.fromList [x,y]) -- now we delete the minNodepair that was found from fSet
       fSet_tmp2 = addNode v fSet_tmp1                      -- add the newly created node to both fSet and vSet 
       vSet_tmp1 = addNode v vSet
       eMap_tmp1 = Map.union (addEdges x v eMap) (addEdges y v eMap) 
       -- create edges from removed nodes to the newly created node v(i) 
       dMap_tmp1 = updateDistMatrix v (x,y) fSet_tmp1 dMap   
       -- Update the distanceMatrix according to given algorithm. ATTENTION fSet with the 
       -- old nodes x,y removed is sent in as param. And not the fSet with new node v added! 
       in loopNJ fSet_tmp2 dMap_tmp1 vSet_tmp1 eMap_tmp1 (count+1)
  | otherwise = let  
       vSet_Final = addNode v vSet
       list = Set.elems fSet
       eMap_tmp1 = Map.union (addEdges (list !! 0) v eMap) (addEdges (list !! 1) v eMap)      
       eMap_Final = addEdges (list !! 2) v eMap_tmp1
       in (vSet_Final,eMap_Final)
  where  
       v = 'v':show count -- create the new node


-- getDistance gives us the distance as a float between two nodes x
-- Sets the default value to 0 if the lockup would return "Nothing"
getDistance :: NodePair-> DistanceMap -> Float
getDistance nodePair map = fromMaybe 0.0 (Map.lookup nodePair map) 

-- SelectMinNodePair takes a nodeset and a map, uses the sFunction to select a pair of nodes (x,y) 
-- in the node set which minimizes the "S function".
selectMinNodePair :: Vertices -> DistanceMap -> NodePair 
selectMinNodePair fset map = (x,y)
  where 
    leaflist = Set.toList fset
    slist    = rmSelfConnected $ [(x,y,sFunction (x,y) fset map)| x<-leaflist,y<-leaflist]
    (x,y,z)  = minNode slist

-- See DOCUMENTATION SECTION (3)   
sFunction :: NodePair -> Vertices -> DistanceMap -> Float                                                         
sFunction (x,y) fset map  = (fromIntegral(Set.size fset -2))*(getDistance (x,y) map) - sum list
                            where list = [getDistance (x,z) map + getDistance (y,z) map | z <- Set.toList fset]

-- Given the computed list of nodes coupled with their S-values, minNode selects the tuple (x,y,z)  
-- with the minimum z, z = S(x,y0). Used as help function to selectMinNodePair
minNode :: [(String,String,Float)] -> (String,String,Float) 
minNode (head:tail)  = myMin head tail 
                       where
                         myMin (s1,s2,f1) [] = (s1,s2,f1)
                         myMin (s1,s2,f1) ((str1,str2,float2):tail) | f1<=float2 = myMin (s1,s2,f1) tail
                                                                    | otherwise  = myMin (str1,str2,float2) tail
  
-- Removes nodes where the leaves are connected to themselves such as ("x","x",_)
-- Used as help function to selectMinNodePair                         
rmSelfConnected :: [(String,String,Float)] -> [(String,String,Float)] 
rmSelfConnected []=[]
rmSelfConnected ((s1,s2,f1):tail)  | s1==s2    = rmSelfConnected tail
                                   | otherwise = (s1,s2,f1):rmSelfConnected tail      

-- See DOCUMENTATION SECTION (1)
updateDistMatrix :: Node -> NodePair -> Vertices -> DistanceMap -> DistanceMap
updateDistMatrix v (x,y) fset map = Map.union map newMap
                                    where
                                      newMap = Map.fromList (left ++ right)
                                      left =  [((node,v),dist node) | node <- Set.toList fset]
                                      right = [((v,node),dist node) | node <- Set.toList fset]
                                      dist node = ((getDistance (node,x) map) + (getDistance (node,y) map)) / 2.0

-- We find our root by taking the list difference betwen the node set and the keys 
-- since the root should be the one which is listed as a node but not as key, because  
-- there should not be any edges from the root to any other nodes.
findOurRoot :: Vertices -> EdgeMap-> Node
findOurRoot nodes edges = head(Set.toList nodes \\ Map.keys edges) 


-- createNodeSet takes a distance matrix and creates a set of strings/vertices no duplicates.
createVerticeSet :: DistanceMatrix -> Vertices
createVerticeSet dm = Set.fromList([s2|(_,s2,_) <- head(dm)])

getDistanceMap ::DistanceMatrix -> DistanceMap
getDistanceMap dm = Map.fromList [((x,y),z) |(x,y,z) <- (concat dm)]
                    
-- deleteNode takes a node and a set and deletes the node from the set
deleteNode :: Node -> Vertices -> Vertices
deleteNode node fset = Set.delete node fset

-- addNode takes a node and a set and adds the node to the set.
addNode :: Node -> Vertices -> Vertices
addNode node fset = Set.insert node fset

-- The structure of how we add/store edges is very important. x represents the node to be deleted 
-- and v is the newly created node. So the key for the map is the "deleted node" x to the edge (x,v)
-- so we will always have the left node x in the tuple (x,v) as a key to the maps value (x,v)
addEdges :: Node -> Node -> EdgeMap-> EdgeMap
addEdges x v edgeMap = Map.insert x (x,v) edgeMap

-- Create a Dist. matrix of  foxp4 & famX sequences, using "distanceMatrix" from mod. Evol. Used as test data! 
matrisFoxp4 :: DistanceMatrix
matrisFoxp4 = distanceMatrix (seqdata foxp4) 
matrisFam1 = distanceMatrix (seqdata fam1) 
matrisFam2 = distanceMatrix (seqdata fam2) 
matrisFam3 = distanceMatrix (seqdata fam3) 
matrisFam4 = distanceMatrix (seqdata fam4) 
matrisFam5 = distanceMatrix (seqdata fam5) 

d1 :: [[(String,String,Float)]]
d1 = [ [("a","a",0.0),("a","b",2),("a","c",3),("a","d",5),("a","e",6),("a","f",5)],
       [("b","a",2.0),("b","b",0),("b","c",3),("b","d",5),("b","e",6),("b","f",5)],
       [("c","a",3),("c","b",3),("c","c",0),("c","d",4),("c","e",5),("c","f",4)],
       [("d","a",5),("d","b",5),("d","c",4),("d","d",0),("d","e",5),("d","f",4)],
       [("e","a",6),("e","b",6),("e","c",5),("e","d",5),("e","e",0),("e","f",3)],
       [("f","a",5),("f","b",5),("f","c",4),("f","d",4),("f","e",3),("f","f",0)]]
  

newick :: (Vertices, EdgeMap) -> String
newick (v, e) = processTree (findOurRoot v e) e

processTree :: Node -> EdgeMap-> String
processTree v e | length edges == 0 = v
                | length edges == 2 = "(" ++ (processTree lt e) ++ " , " ++ (processTree rt e) ++ ")"                     
                | length edges == 3 = "(" ++ (processTree lt e) ++ " , " ++ (processTree mt e) ++ " , " ++ (processTree rt e) ++ ")"                      
                | otherwise = error ("Hmm sth went wrong in processTree")                      
                 where edges= [(x,(y,z))| (x,(y,z)) <- Map.toList e, v==z]
                       lt = if null edges then "" else fst $ edges !! 0
                       rt = if null edges then "" else fst $ edges !! 1
                       mt = if null edges then "" else fst $ edges !! 2

                       
-- ************* THIS IS THE DOCUMENTATION SECTION **************
{-
(1).
Avståndsmatrisen är uppbyggd på så sätt att den första string elementet i tupeln som representarer namnet på sekvensen
(de som bildar våra lövar) hålls fixt och den andra string. elem. som representarar namnet på de andra sekvensen 
itereras över alla sekvenser. Så bildas en rad i matrisen ex. [[(a,a,_),(a,b,_),(a,c,_),..], [(b,a,_),(b,b,_),..],..]

(2).
updateMatrix takes the newly created node v, the removed nodePair (x,y), the node set fset with x and y removed 
and the current distance map/matrix. The function then creates commutative connections i.e (v,node) and (node,v)
from the new node v to all other remaining nodes in the nodeset and it calculuates a distance for the new
conectitions based on the distance of the old removed nodes x,y.  Given by: D(v,node)= D(node,x) + D(node,y) div by 2.
It saves all this info to a list which it then constructs a map from, and then joins the old map with newly created map.

(3).
The sFunction implements the special S-function given in the specification. Given a nodepair,
a set of nodes and a map as parameters, it calculates some kind of distance between the two
nodes (x,y). In general S(x,y) notequal to S(y,x). Used as help function in selectMinNodePair  

(4).
getDistanceMap converts a DistanceMatrix to a DistanceMap. It takes a matrix, flattens it to a single list & then creates 
a map from the list with the nodes as keys and the float as the value. Structure of an element is: ((String,String),Float)
-}



