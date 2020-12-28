module Main where

import qualified CombinatorialOptimisation.TSP as TSP
import qualified FileFormat.TSPLIB as FF 
import qualified TSPsolution as S
import qualified System.Environment as SE (getArgs)

solvefile tspfile  =do tsp <- FF.loadTSPFile TSP.ExplicitMatrix (tspfile)
                       putStr (showSolution tsp)
                        where showSolution = S.showSolution


main :: IO ()
main = do 
--    let tspPath = "/home/loxymondor/docs/facu/4th/sem1/AI/proj/TSP-Problems/"
--    let file = "problem1.tsp" 
--    let file = "problem4.atsp" 
--    x <- FF.loadTSPFile TSP.ExplicitMatrix (tspPath++file)
      args <- SE.getArgs
      case args of 
         [tspfile] -> solvefile tspfile        
--          tspfiles  -> map solvefile tspfiles
         _         -> putStrLn "error in args." 
