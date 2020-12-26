module TSPsolution (solve,showSolution) where

import qualified CombinatorialOptimisation.TSP as TSP
import qualified Data.List as DL (foldl') 

showSolution :: TSP.TSPProblem -> String 
showSolution a = show (solve a [1..((TSP.numCities a)-1)] 0 )

kick :: (Eq a) => a -> [a] -> [a]
kick n xs = filter (/=n) xs

nextMin :: TSP.TSPProblem -> Int -> [Int] -> Int
nextMin tsp sp uv = fst ( DL.foldl' minWeight (sp,inf) uv) 
   where 
--           minWeight ::  Floating a =>  (Ord (Int -> a ))  => (Int,Int -> a) -> Int -> (Int,Int -> a)
         inf=9999
         minWeight a@(_,w) b       | w < w'    = a
                                   | otherwise = b'
          where w' = TSP.edgeCost tsp sp b
                b' = (b,w') 


solve :: TSP.TSPProblem  -> [Int] -> Int -> [Int]

solve _ [] _   = [] 

solve problem unvisted startpoint = [startpoint ] ++ (solve problem unvisted' startpoint' ) 
      where 
            startpoint' = (nextMin problem startpoint unvisted)
            unvisted' = (kick startpoint unvisted) 
--             unvisted' = kick startpoint' (kick startpoint unvisted) 
-- ((maximum).(map (TSP.edgeCost p s))) uv 


-- solve i = ((maximum).(TSP.edgeCost i 0)) [0..(TSP.numCities-1)] 
