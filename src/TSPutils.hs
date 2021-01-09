-- module TSPsolution (solve,showSolution,multiSolve,costOP,costOfPath) where
module TSPutils where

import qualified CombinatorialOptimisation.TSP as TSP
import qualified Data.List as DL (foldl') 
import qualified Data.Tuple as DT (swap)
import qualified Types as Tp 

kick :: (Eq a) => a -> [a] -> [a]
kick n xs = filter (/=n) xs

type Cost = Double  
type Node = Int
type CPath = [(Node,Cost)]
type Path = [Node]


pathCost :: CPath -> Cost
pathCost path = DL.foldl' costSum 0  path 
      where costSum sum node = sum + (snd node)

costOfPath :: TSP.TSPProblem -> Tp.PathType -> Tp.Direction  -> Path -> Cost
costOfPath _ _   _   []         = 0  
costOfPath p typ dir path@(h:t) = DL.foldl' costof 0 path'
      where path' = case typ of 
                     Tp.Circle -> zip (path) (t++[h])
                     Tp.Line   -> zip (path) (t) 
-- needs support for direction         
--                         where swap a = a
            costof acc link@(a,b) = acc + (TSP.edgeCost p a b)

costOP p path = costOfPath p Tp.Circle Tp.Forward path 


nextMin :: TSP.TSPProblem -> Node -> [Node] -> Node
nextMin tsp sp uv = fst . DL.foldl' minWeight (sp,inf) $ uv 
      where 
         inf = 9999999
--          minWeight ::  Floating a =>  (Ord (Int -> a ))  => (Int,Int -> a) -> Int -> (Int,Int -> a)
         minWeight a@(_,w) b       | w < w'    = a
                                   | otherwise = b'
          where w' = TSP.edgeCost tsp sp b
                b' = (b,w') 
-- nextMin _ _ [] = 
pQ :: TSP.TSPProblem -> Node -> [Node] -> [Node]
-- untested priority queue
pQ tsp n uv = h : pQ tsp n uv'
      where h       = minNode uv
            minNode = nextMin tsp n 
            uv'     = kick h uv

chooseLCPath :: [CPath] -> CPath  
-- choose least cost path
chooseLCPath [] = []
chooseLCPath [a] = a 
chooseLCPath paths@(h:t) = fst . DL.foldl' minCostCPath h' $ t
      where 
            h' = (h , (pathCost h ) )

minCostCPath :: (CPath,Cost) -> CPath -> (CPath,Cost) 

minCostCPath n@(_,c) p' | c' < c     = (p',c')
                        | otherwise  = n 
      where c' = (pathCost p')

normalizePath :: CPath -> Node -> CPath
-- make any path start from City #0 
normalizePath path root = dropWhile root' path ++ takeWhile root' path 
           where root'  (n',_) | root/=n'  = True
                               | otherwise = False  

solve :: TSP.TSPProblem  -> [Node] -> Node -> Node -> CPath
-- NN solver : choose least cost link and jump to .
--  no backtracking
--this one impelements NN heuristic
solve _ []  _  _            = [] 
solve p [a] r  s  | a == s  = [(a,TSP.edgeCost p a r )]

solve problem unvisted root startpoint 
      = [(startpoint, costOfNextStep) ] 
             ++
        (solve problem unvisted' root startpoint' ) 
      where 
         unvisted' = (kick startpoint unvisted) 
         startpoint' = (nextMin problem startpoint unvisted')
         costOfNextStep = TSP.edgeCost problem startpoint startpoint'


multiSolve ::  TSP.TSPProblem  -> [Node] -> CPath
-- multi start point solve
---------------------------
-- solve using all points as a start point
-----
--
multiSolve p nodes =  normalizePath (chooseLCPath paths) 0
      where paths = (map (\sp -> solve p nodes sp sp) nodes )

