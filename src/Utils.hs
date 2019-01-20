module Utils where

import qualified Data.Text as T
import Control.Lens 
import qualified Data.Vector as Vector

showT :: Show a => a -> T.Text
showT = T.pack . show

iforM_ :: Monad m => Vector.Vector a -> (Int -> a -> m b) -> m () 
iforM_ = flip Vector.imapM_
