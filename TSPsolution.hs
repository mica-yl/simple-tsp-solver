module TSPsolution (solve) where

import qualified CombinatorialOptimisation.TSP as TSP
showSolution :: [a] -> String 
showSolution a = "solution"


-- solve :: TSP.TSPProblem -> [Int]
-- solve i = undefined
solve i = TSP.showEdgeWeights i

