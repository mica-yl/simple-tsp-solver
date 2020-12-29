-- module TSPsolution (solve,showSolution,multiSolve,costOP,costOfPath) where
module TSPsolution where

import qualified CombinatorialOptimisation.TSP as TSP
import qualified Data.List as DL (foldl',unlines) 
import qualified Data.Tuple as DT (swap)
import qualified Types as Tp 

showSolution :: TSP.TSPProblem -> String 
showSolution problem = ((DL.unlines . (map show2T)) solution )
                 ++
                 "\n Total cost :" ++ (show (pathCost solution))
  where 
--         solution =(solve a [0..((TSP.numCities a)-1)] 0 0 )
        solution =(multiSolve problem [0..((TSP.numCities problem)-1)])
        show2T (a,b) = (show a) ++ "\t" ++ (show b) 



kick :: (Eq a) => a -> [a] -> [a]
kick n xs = filter (/=n) xs

type Cost = Double  
type Node = Int
type Path = [(Node,Cost)]


pathCost :: Path -> Cost
pathCost path = DL.foldl' costSum 0  path 
      where costSum sum node = sum + (snd node)

costOfPath :: TSP.TSPProblem -> Tp.PathType -> Tp.Direction  -> [Node] -> Cost
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
nextMin tsp sp uv = fst ( DL.foldl' minWeight (sp,inf) uv) 
      where 
         inf = 9999999
--          minWeight ::  Floating a =>  (Ord (Int -> a ))  => (Int,Int -> a) -> Int -> (Int,Int -> a)
         minWeight a@(_,w) b       | w < w'    = a
                                   | otherwise = b'
          where w' = TSP.edgeCost tsp sp b
                b' = (b,w') 
-- nextMin _ _ [] = 


solve :: TSP.TSPProblem  -> [Node] -> Node -> Node -> Path
solve _ []  _  _            = [] 
solve p [a] r  s  | a == s  = [(a,TSP.edgeCost p a r )]

solve problem unvisted root startpoint = [(startpoint, costOfNextStep) ] 
                                         ++
                                         (solve problem unvisted' root startpoint' ) 
      where 
            unvisted' = (kick startpoint unvisted) 
            startpoint' = (nextMin problem startpoint unvisted')
            costOfNextStep = TSP.edgeCost problem startpoint startpoint'

-- multi start point solve
---------------------------
-- solve using all points as a start point

multiSolve ::  TSP.TSPProblem  -> [Node] -> Path
multiSolve p nodes =  normalizePath (chooseLCPath paths) 0
      where paths = (map (\sp -> solve p nodes sp sp) nodes )

chooseLCPath :: [Path] -> Path  
chooseLCPath [] = []
chooseLCPath [a] = a 
chooseLCPath paths@(h:t) = fst (DL.foldl' minCost h' t)
      where 
            h' = (h , (pathCost h ) )
-- try it with @[h:t] to use head as the base case

minCost :: (Path,Cost) -> Path -> (Path,Cost) 
minCost n@(_,c) p' | c' < c     = (p',c')
                   | otherwise  = n 
      where c' = (pathCost p')

normalizePath :: Path -> Node -> Path
normalizePath path root = (dropWhile root' path)++(takeWhile root' path) 
           where root'  (n',_) | root/=n'  = True
                               | otherwise = False  
