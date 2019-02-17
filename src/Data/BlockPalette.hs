
module Data.BlockPalette (BlockPalette, HasBP(..), newBlockPalette, getMCName, getBlockId, prettyPrintBlockPalette, doPaletteSwap) where

import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.IORef
import Data.Word
import qualified Data.HashTable.IO as HashTable
import Control.Monad.IO.Class
import Control.Monad.Reader.Class

import Logging.Contextual
import Logging.Contextual.BasicScheme

type HashTable k v = HashTable.BasicHashTable k v

data BlockPalette = BlockPalette
 { toMCName :: !(HashTable Word16 T.Text)
 , fromMCName :: !(HashTable T.Text Word16)
 , nextIdRef :: !(IORef Word16)
 }

class HasBP a where
  getBP :: a -> BlockPalette

type BP env m = (MonadReader env m, HasLog env, HasBP env, MonadIO m)

askBP :: (MonadReader env m, HasBP env) => m BlockPalette
askBP = fmap getBP ask

newBlockPalette :: (HasLog env, MonadReader env m, MonadIO m) => m BlockPalette
newBlockPalette = do
  [logTrace|creating new block palette|]
  liftIO $ BlockPalette <$> HashTable.new <*> HashTable.new <*> newIORef 0

getBlockId :: (BP env m) => T.Text -> m Word16
getBlockId mcName = do
  BlockPalette {toMCName, fromMCName, nextIdRef } <- askBP
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

getMCName :: (BP env m) => Word16 -> m T.Text
getMCName blockId = do
  BlockPalette {toMCName} <- askBP
  result <- liftIO $ HashTable.lookup toMCName blockId
  case result of
    Nothing -> error $ "failed to lookup block id " <> show blockId
    Just mcName -> return mcName

doPaletteSwap :: (BP env m) => T.Text -> T.Text -> m ()
doPaletteSwap oldMCBlockId newMCBlockId = do
  BlockPalette {toMCName, fromMCName} <- askBP
  liftIO $
    HashTable.lookup fromMCName oldMCBlockId >>= \case
      Nothing -> error "no such block id"
      Just blockId -> do
        HashTable.insert toMCName blockId newMCBlockId
        HashTable.delete fromMCName oldMCBlockId
        HashTable.insert fromMCName newMCBlockId blockId


prettyPrintBlockPalette :: (BP env m) =>  m ()
prettyPrintBlockPalette = do
  BlockPalette { fromMCName } <- askBP
  liftIO $ flip HashTable.mapM_ fromMCName $ \(k, v) ->
    T.putStrLn $ k <> " --> " <> T.pack (show v)