module Lib (
    Graph,
    runDijkstra,
    testGraph
    ) where

import Prelude hiding (pred)
import Data.Map.Strict as M
import Data.Foldable (minimumBy)

type Graph a = Map a (Map a Int)

testGraph :: Graph Char
testGraph = fromList [
    ('A', fromList [('B', 1), ('C', 2)]),
    ('B', fromList [('D', 7)]),
    ('C', fromList [('D', 5)]),
    ('D', fromList [])]

data Node a = Node { 
    visited :: Bool
  , distance :: Int
  , pred :: Maybe a }

-- | Return the shortest distances from an origin to all other nodes and for
-- each node its predecessor in the shortest path.
runDijkstra :: Ord a => Graph a -> a -> Map a (Int, Maybe a)
runDijkstra g o = 
    let distances = mapWithKey 
            (\k _ ->  Node False (if k == o then 0 else maxBound) Nothing) g 
     in go distances where
        go m = if all visited m
           then fmap (\n -> (distance n, pred n)) m
           else 
            let (c, n) = minimumBy 
                    (\(_, n1) (_, n2) -> compare (distance n1) (distance n2))
                    $ toList $ M.filter (not . visited) m
                neighbors = filterWithKey 
                    (const . not . visited . (m !)) g ! c
                m' = adjust (\n -> n { visited = True }) c m
             in go $ foldrWithKey (f c n) m' neighbors where
                 f c n = \c' d' m' -> 
                     let d1 = distance n + d' 
                         update = \n' -> 
                            if d1 < distance n' 
                               then n' { distance = d1, pred = Just c } 
                               else n'
                      in adjust update c' m'

