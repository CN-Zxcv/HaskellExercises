
import System.Random hiding (next)
import Control.Arrow

import Supply


randomsIO :: Random a => IO [a]
randomsIO =
    getStdRandom $ \g ->
        let (a, b) = split g
        in (randoms a, b)


randomsIO_golfed :: Random a => IO [a]
randomsIO_golfed = getStdRandom (first randoms . split)

a :: Random s => IO (Maybe s)
a = (fst . runSupply next) `fmap` randomsIO
