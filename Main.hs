import qualified CombinatorialOptimisation.TSP as TSP
import qualified FileFormat.TSPLIB as FF 
import qualified TSPsolution as S

-- read :: IO a -> IO b
main = do 
   let tspPath = "/home/loxymondor/docs/facu/4th/sem1/AI/proj/TSP-Problems/"
--    let file = "problem1.tsp" 
   let file = "problem4.atsp" 
   x <- FF.loadTSPFile TSP.ExplicitMatrix (tspPath++file)
   putStr (showSolution x)
      where showSolution = S.showSolution 
