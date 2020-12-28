module TSPsolution (solve,showSolution) where

import qualified CombinatorialOptimisation.TSP as TSP
import qualified Data.List as DL (foldl',unlines) 

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

nextMin :: TSP.TSPProblem -> Node -> [Node] -> Node
nextMin tsp sp uv = fst ( DL.foldl' minWeight (sp,inf) uv) 
   where 
         inf = 9999
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


multiSolve ::  TSP.TSPProblem  -> [Node] -> Path
multiSolve p nodes = choosePath (map (\sp -> solve p nodes sp sp) nodes )
      where choosePath :: [Path] -> Path  
            choosePath paths = fst (DL.foldl' minCost dummy paths)
             where dummyPath = (last paths)
                   dummy = (dummyPath , (pathCost dummyPath ) )
--                    minCost :: Floating a => [(Int,a)] -> [(Int,a)] -> (a,[(Int,a)]) 
                   minCost :: (Path,Cost) -> Path -> (Path,Cost) 
                   minCost n@(_,c) p' | c' < c     = (p',c')
                                      | otherwise  = n 
                        where c' = (pathCost p')
