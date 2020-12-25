module TSPsolution (solve,showSolution) where

import qualified CombinatorialOptimisation.TSP as TSP
showSolution :: TSP.TSPProblem -> String 
showSolution a = show (solve a)


-- solve :: TSP.TSPProblem -> [Floating a]
solve i =  
      where findMin  = (maximum).(map (TSP.edgeCost i 0)) [0..((TSP.numCities i)-1)] 
-- solve i = ((maximum).(TSP.edgeCost i 0)) [0..(TSP.numCities-1)] 
