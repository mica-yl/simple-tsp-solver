-- module TSPsolution (solve,showSolution,multiSolve,costOP,costOfPath) where
module PrettySolution where

import qualified CombinatorialOptimisation.TSP as TSP
import qualified Data.List as DL (unlines) 
import qualified Types as Tp 
import qualified TSPutils as U 

showSolution :: TSP.TSPProblem -> String 
showSolution problem = (DL.unlines . map show2T $ solution )
                 ++
                 "\n Total cost :" ++ (show (U.pathCost solution))
  where 
        solution =(U.multiSolve problem [0..((TSP.numCities problem)-1)])
        show2T (a,b) = (show a) ++ "\t" ++ (show b) 

