module TSPsolution (solve,showSolution) where

import qualified CombinatorialOptimisation.TSP as TSP
import qualified Data.List as DL (foldl',unlines) 

showSolution :: TSP.TSPProblem -> String 
showSolution a = (DL.unlines . (map show2T)) solution
  where solution = (solve a [1..((TSP.numCities a)-1)] 0 )
        show2T (a,b) = (show a) ++ "\t" ++ (show b) 

kick :: (Eq a) => a -> [a] -> [a]
kick n xs = filter (/=n) xs

nextMin :: TSP.TSPProblem -> Int -> [Int] -> Int
nextMin tsp sp uv = fst ( DL.foldl' minWeight (sp,inf) uv) 
   where 
--          minWeight ::  Floating a =>  (Ord (Int -> a ))  => (Int,Int -> a) -> Int -> (Int,Int -> a)
         inf = 9999
         minWeight a@(_,w) b       | w < w'    = a
                                   | otherwise = b'
          where w' = TSP.edgeCost tsp sp b
                b' = (b,w') 
-- nextMin _ _ [] = 

solve :: Floating a => TSP.TSPProblem  -> [Int] -> Int -> [(Int,a)]

solve _ [] _   = [] 

solve problem unvisted startpoint = [(startpoint, costOfNextStep) ] 
                                    ++
                                    (solve problem unvisted' startpoint' ) 
      where 
            unvisted' = (kick startpoint unvisted) 
            startpoint' = (nextMin problem startpoint unvisted')
            costOfNextStep = TSP.edgeCost problem startpoint startpoint'
--             unvisted' = kick startpoint' (kick startpoint unvisted) 
-- ((maximum).(map (TSP.edgeCost p s))) uv 


-- solve i = ((maximum).(TSP.edgeCost i 0)) [0..(TSP.numCities-1)] 
