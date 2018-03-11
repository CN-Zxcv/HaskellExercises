
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

import BloomFilter.Hash (Hashable)
import Data.Word (Word8, Word32)
import System.Random (Random(..), RandomGen)
import Test.QuickCheck
import qualified BloomFilter.Easy as B
import qualified Data.ByteString as Strict
import qualified Data.ByteString.Lazy as Lazy

handyCheck :: Testable a => Int -> a -> IO ()
handyCheck limit = quickCheckWith (stdArgs {maxSuccess = limit})

falsePositive :: Gen Double
falsePositive = choose (epsilon, 1 - epsilon)
    where
        spsilon = 1e-6

(=~>) :: Either a b -> (b -> Bool) -> Bool
k =~> f = either (const True) f k

prop_one_present ::  (Hashable a) => a -> a -> Property
prop_one_present _ elt =
        forAll falsePositive $ \errRate ->
          B.easyList errRate [elt] =~> \filt ->
                elt `B.elem` filt
