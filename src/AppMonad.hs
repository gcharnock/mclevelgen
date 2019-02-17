
module AppMonad where

import           Control.Monad.Trans.Reader
import           Logging.Contextual
import           Data.BlockPalette

data AppContext = AppContext 
  { blockPalette :: BlockPalette
  , logger :: Logger
  }

instance HasBP AppContext where
    getBP = blockPalette

instance HasLog AppContext where
    getLog = logger
    setLog logger' app = app { logger = logger' }

type App = ReaderT AppContext IO
