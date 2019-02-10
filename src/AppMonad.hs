
module AppMonad where

import Control.Monad.Trans.Reader
import Logging.Contextual

data AppContext = AppContext {
    logger :: Logger
}

instance HasLog AppContext where
    getLog = logger
    setLog logger' app = app {logger = logger'}

type App = ReaderT AppContext IO
