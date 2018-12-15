module Utils where

import Control.Lens 
import qualified Data.Vector as Vector

iforM_ :: Monad m => Vector.Vector a -> (Int -> a -> m b) -> m () 
iforM_ = flip Vector.imapM_
