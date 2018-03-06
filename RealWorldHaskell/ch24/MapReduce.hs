
module MapReduce (
      mapReduce
    ) where

import Control.Parallel.Strategies
import GHC.Conc

simpleMapReduce
    :: (a -> b)     -- map function
    -> ([b] -> c)   -- reduce function
    -> [a]          -- list to map over
    -> c
simpleMapReduce mapFunc reduceFunc = reduceFunc . map mapFunc

mapReduce
    :: Strategy b   --
    -> (a -> b)
    -> Strategy c
    -> ([b] -> c)
    -> [a]
    -> c
mapReduce mapStrat mapFunc reduceStrat reduceFunc input =
    mapResult `pseq` reduceResult
    where
        mapResult = parMap mapStrat mapFunc input
        reduceResult = reduceFunc mapResult `using` reduceStrat
