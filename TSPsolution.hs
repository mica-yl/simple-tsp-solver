module TSPsolution (solve,showSolution) where

import qualified CombinatorialOptimisation.TSP as TSP
showSolution :: TSP.TSPProblem -> String 
showSolution a = show (solve a [0..((TSP.numCities a)-1)] 0 )

kick :: (Eq a) => a -> [a] -> [a]
kick n xs = filter (/=n) xs

solve :: TSP.TSPProblem  -> [Int] -> Int -> [Int]

solve _ _ (-1) = [] 

solve problem unvisted startpoint = [startpoint ] ++ (solve problem (kick startpoint unvisted) (nextMin problem startpoint unvisted)) 
      where nextMin p s uv = head ( filter (==s+1) uv )
             where head [] = -1
                   head a  = Prelude.head a  

-- ((maximum).(map (TSP.edgeCost p s))) uv 

solve _ [] _ = [] 

-- solve i = ((maximum).(TSP.edgeCost i 0)) [0..(TSP.numCities-1)] 
