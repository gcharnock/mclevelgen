module Utils where

import qualified Data.Text as T
import Control.Lens 
import qualified Data.Vector as Vector
import Data.Array

showT :: Show a => a -> T.Text
showT = T.pack . show

iforM_ :: Monad m => Vector.Vector a -> (Int -> a -> m b) -> m () 
iforM_ = flip Vector.imapM_

listToArray :: (Num i, Ix i) => [e] -> Array i e
listToArray l = listArray (0, fromIntegral (length l) - 1) l
