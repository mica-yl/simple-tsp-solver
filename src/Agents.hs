-- module TSPsolution (solve,showSolution,multiSolve,costOP,costOfPath) where
module Agents where

import qualified CombinatorialOptimisation.TSP as TSP
import qualified Data.List as L (foldl',(\\),mapAccumL) 
import qualified Data.Tuple as T (swap)
import qualified Types as Tp 
import qualified TSPutils as U

type Cost = Double  
type Node = Int
type CPath = [(Node,Cost)]
type Path = [Node]

genericSolver :: TSP.TSPProblem -> Path

genericSolver p = genericSolver' p sortNext qF 
                   ( dummyPath , it'sCost) 0 
                   visited startpoint  
      where 
            sortNext   = U.pQ p
            qF         = (\c c' -> c < c')
            dummyPath  = U.allCities p  
            it'sCost   = U.costOfPath p Tp.Circle Tp.Forward dummyPath
            startpoint = 0
            visited    = []
                

genericSolver' :: TSP.TSPProblem -- problem
      -> ( Node -> [Node] -> [Node])          -- sort next nodes
      -> (Cost-> Cost -> Bool)   -- abort when path's cost exceds best path
      -> (Path,Cost) -> Cost     -- best Solution , Cost of current path
      -> [Node] -> Node          -- the current path , current node
      -> Path                    -- -> output 
 

-- quitting search 
genericSolver' _ _ quitFunc bs@(p,c) c' p' _       
      | quitFunc c c' = p  

-- final : end of successfull search 
genericSolver' _ _ _ bs@(p,c) c'  p' _ 
      | length p' == length p = p'

-- main
genericSolver' p sortNext quitFunc bestSol totalCost visited curNode 
 = mapSolver genericSolver'' nextNodes
 where 
       mapSolver :: ((Path,Cost) -> Node -> (Path,Cost)) -> [Node] -> Path
       mapSolver s l = fst . L.foldl' s bestSol $ l
       genericSolver'' bs nextNode
          = (nextPath , it'sCost )
        where 
              nextPath    = genericSolver' 
                             p sortNext quitFunc
                             bs newCost v' nextNode
              v'      = visited ++ [nextNode] 
              newCost = totalCost 
                             + (TSP.edgeCost p curNode nextNode)                 
              it'sCost   = U.costOfPath p Tp.Circle Tp.Forward nextPath
                
       nextNodes = sortNext curNode uv
            where uv = (U.allCities p) L.\\ visited 
            -- exclude visited nodes from allCities
 
