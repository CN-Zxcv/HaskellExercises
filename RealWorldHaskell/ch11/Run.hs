module Main where

import Test.QuickCheck
import Control.Monad (forM_)

import Prettify2
import QC
-- import Test.QuickCheck.Batch

runTests :: String -> Args -> [Run] -> IO ()
runTests name opts tests = putStrLn ("Running " ++ name ++ " tests:") >> forM_ tests (\rn -> rn opts)

type Run = Args -> IO ()

run :: Testable prop => prop -> Run
run = flip quickCheckWith

--options = TestOptions
--      { no_of_tests         = 200
--      , length_of_tests     = 1
--      , debug_tests         = False }

options = stdArgs { maxSuccess = 200, maxSize = 200}

main = do
    runTests "simple" options
        [ run prop_empty_id
        , run prop_char
        , run prop_text
        , run prop_line
        , run prop_double
        ]

    runTests "complex" options
        [ run prop_hcat
        , run prop_punctuate'
        ]

{-
ghc -fhpc Run.hs --make
    $ hpc report Run --exclude=Main --exclude=QC
     18% expressions used (30/158)
      0% boolean coverage (0/3)
           0% guards (0/3), 3 unevaluated
         100% 'if' conditions (0/0)
         100% qualifiers (0/0)
     23% alternatives used (8/34)
      0% local declarations used (0/4)
     42% top-level declarations used (9/21)
-}
{-
hpc markup Run --exclude=Main --exclude=QC
    hpc_index.html into a browser
-}
