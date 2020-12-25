import qualified CombinatorialOptimisation.TSP as TSP
import qualified FileFormat.TSPLIB as FF 
import qualified TSPsolution as S

-- read :: IO a -> IO b
main = do 
   let file = "/home/loxymondor/docs/facu/4th/sem1/AI/proj/TSP-Problems/problem1.tsp" 
   x <- FF.loadTSPFile TSP.ExplicitMatrix file
   putStr (showSolution x)
      where showSolution = S.solve
