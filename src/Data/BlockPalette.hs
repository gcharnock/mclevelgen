
module Data.BlockPalette (BlockPalette, newBlockPalette, getMCName, getBlockId, prettyPrintBlockPalette) where

import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.IORef
import Data.Word
import qualified Data.HashTable.IO as HashTable
import qualified Utils
import Control.Monad.IO.Class

import Logging.Contextual.BasicScheme
import AppMonad

type HashTable k v = HashTable.BasicHashTable k v

data BlockPalette = BlockPalette
 { toMCName :: !(HashTable Word16 T.Text)
 , fromMCName :: !(HashTable T.Text Word16)
 , nextIdRef :: !(IORef Word16)
 }

newBlockPalette :: App BlockPalette
newBlockPalette = do
  [logTrace|creating new block palette|]
  liftIO $ BlockPalette <$> HashTable.new <*> HashTable.new <*> newIORef 0

getBlockId :: BlockPalette -> T.Text -> App Word16
getBlockId BlockPalette { nextIdRef, toMCName, fromMCName } mcName = do
  result <- liftIO $ HashTable.lookup fromMCName mcName
  case result of
    Just blockId -> return blockId
    Nothing -> do
      nextId <- liftIO $ readIORef nextIdRef
      [logInfo|New block id found: {mcName}, assigning id: {nextId}|]
      liftIO $ HashTable.insert toMCName nextId mcName
      liftIO $ HashTable.insert fromMCName mcName nextId
      liftIO $ writeIORef nextIdRef (nextId + 1)
      return nextId

getMCName :: BlockPalette -> Word16 -> App T.Text
getMCName BlockPalette { toMCName } blockId = do
  result <- liftIO $ HashTable.lookup toMCName blockId
  case result of
    Nothing -> error $ "failed to lookup block id " <> show blockId
    Just mcName -> return mcName

prettyPrintBlockPalette :: BlockPalette -> IO ()
prettyPrintBlockPalette BlockPalette { fromMCName } =
  flip HashTable.mapM_ fromMCName $ \(k, v) -> T.putStrLn $ k <> " --> " <> T.pack (show v)