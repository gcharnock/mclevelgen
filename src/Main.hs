{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

import           Control.Monad
import           Anvil
import           Data.Region
import           Data.Chunk
import           Data.BlockPalette
import qualified Data.Vector.Unboxed.Mutable   as MVector
import           Logging.Contextual
import           Logging.Contextual.BasicScheme
import           Control.Monad.IO.Unlift
import           Control.Monad.Trans.Reader
import           UnliftIO.IO

import           AppMonad

mutateMap :: Region -> App ()
mutateMap Region { regionChunkMap } = do
    [logInfo|Running mutateMap|]
    planksId    <- getBlockId "minecraft:planks"
    sandstoneId <- getBlockId "minecraft:sandstone"
    forM_ regionChunkMap $ \Chunk { chunkBlocks } -> do
        let len = MVector.length chunkBlocks
        forM_ [0 .. len - 1] $ \i -> do
            blockId <- MVector.read chunkBlocks i
            when (blockId == sandstoneId) $ MVector.write chunkBlocks i planksId

app :: App ()
app = do
    [logHeadline|starting levelgen|]
    region <- withFile "r.0.0.mca" ReadMode $ \handle -> readRegion handle
    -- mutateMap region
    prettyPrintBlockPalette
    doPaletteSwap "minecraft:sandstone" "minecraft:planks"
    prettyPrintBlockPalette
    [logInfo|writing to file|]
    withFile "example/region/r.0.0.mca" WriteMode
        $ \handle -> writeRegion handle region
    return ()


main :: IO ()
main = do
    let loggerSettings = LoggerSettings 4 "localhost" 5432 "postgres" "" "log"
    withLogger loggerSettings (LogEvent "levelgen-run" Nothing)
        $ \logger -> do
            blockPalette <- runReaderT newBlockPalette logger
            runReaderT app (AppContext { logger, blockPalette })
