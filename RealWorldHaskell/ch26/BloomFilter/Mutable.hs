module BloomFilter.Mutable (
      MutBloom
    , elem
    , notElem
    , insert
    , length
    , new
    ) where

import Control.Monad (liftM)
import Control.Monad.ST (ST)
import Data.Array.MArray (getBounds, newArray, readArray, writeArray)
import Data.Word (Word32)
import Prelude hiding (elem, length, notElem)
import BloomFilter.Internal (MutBloom(..))

new :: (a -> [Word32]) -> Word32 -> ST s (MutBloom s a)
new hash numBits = MB hash `liftM` newArray (0, numBits - 1) False

insert :: MutBloom s a -> a -> ST s ()
insert filt elt =
    indices filt elt >>=
    mapM_ (\bit -> writeArray (mutArray filt) bit True)

--insert' :: MutBloom s a -> a -> ST s ()
--insert' filt elt = do
--    hashs <- indices filt elt
--    mapM_ (\bit -> writeArray (mutArray filt) bit True) hashs

indices :: MutBloom s a -> a -> ST s [Word32]
indices filt elt = do
    modulus <- length filt
    return $ map (`mod` modulus) (mutHash filt elt)

elem :: a -> MutBloom s a -> ST s Bool
elem elt filt =
    indices filt elt >>=
    allM (readArray (mutArray filt))

notElem :: a -> MutBloom s a -> ST s Bool
notElem elt filt = not `liftM` elem elt filt

allM :: Monad m => (a -> m Bool) -> [a] -> m Bool
allM p (x:xs) = do
    ok <- p x
    if ok
        then allM p xs
        else return False
allM _ [] = return True

length = undefined
