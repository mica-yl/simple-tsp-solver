module Agents where

import qualified CombinatorialOptimisation.TSP as TSP
import qualified Data.List as L (foldl',(\\),mapAccumL) 
import qualified Data.Tuple as T (swap)
import qualified Types as Tp 
import qualified TSPutils as U
import qualified Debug.Trace as Dbg (trace)


type Cost = Double  
type Node = Int
type CPath = [(Node,Cost)]
type Path = [Node]

genericSolver :: TSP.TSPProblem -> Path

genericSolver p = genericSolver' p sortNext qF 
                   ( dummyPath , it'sCost) 0 
                   visited startpoint  
      where 
--             sortNext   = U.pQ p
            sortNext   = (\ n list -> list) 
            qF         = (\c c' -> c < c')
--             qF         = (\c c' -> False )
            dummyPath  = U.allCities p  
            it'sCost   = U.costOfPath p Tp.Circle Tp.Forward dummyPath
            startpoint = Just 0
            visited    = []
                

genericSolver' :: TSP.TSPProblem -- problem
      -> ( Node -> [Node] -> [Node])          -- sort next nodes
      -> (Cost-> Cost -> Bool)   -- abort when path's cost exceds best path
      -> (Path,Cost) -> Cost     -- best Solution , Cost of current path
      -> [Node] -> Maybe Node    -- the current path , current node
      -> Path                    -- -> output 
 

-- Debug
genericSolver' _ _ _ (p,c) c' p' _ | Dbg.trace dbgMsg False = undefined    
      where dbgMsg = (show p ++ show c ++ "\t" ++ show p' ++ show c')

-- quitting search 
genericSolver' _ _ quitFunc bs@(p,c) c' p' _       
      | quitFunc c c' = Dbg.trace ("quitting :"++ show p ++ (show (c,c'))) p  

-- final : end of successfull search 
genericSolver' tsp _ _ bs@(p,c) _  p' Nothing 
      | betterCost = Dbg.trace ("new Path found :" ++ show p' ++(show c') ) p'
      | otherwise       = Dbg.trace ("old Path :" ++ show p ++ (show (c,c'))) p  
            where sameLen    = ((length p') == (length p))
                  betterCost = c' < c
                  c'         = U.costOfPath tsp Tp.Circle Tp.Forward p'
-- main
genericSolver' p sortNext quitFunc bestSol totalCost visited (Just curNode) 
 = mapSolver genericSolver'' nextNodes
 where 
       visited' = visited ++ [curNode]

       mapSolver :: ((Path,Cost) -> Maybe Node -> (Path,Cost)) -> [Maybe Node] -> Path
       mapSolver s [] = fst . s bestSol $ Nothing 
       mapSolver s l   = fst . L.foldl' s bestSol $ l
       genericSolver'' :: (Path,Cost) -> Maybe Node -> (Path,Cost)
       genericSolver'' bs nextNode
          = (nextPath , it'sCost ) 
        where 
            
              nextPath   = genericSolver' 
                             p sortNext quitFunc
                             bs (newCost nextNode) visited' nextNode
              newCost (Just n)   
                         = totalCost 
                             + (TSP.edgeCost p curNode n)
              newCost (Nothing)
                         = totalCost                 
              it'sCost   = U.costOfPath p Tp.Circle Tp.Forward nextPath
                
       nextNodes = map Just . sortNext curNode $ uv
            where uv = (U.allCities p) L.\\ visited' 
            -- exclude visited nodes from allCities
 
