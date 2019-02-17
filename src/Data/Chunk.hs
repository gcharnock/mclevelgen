{-# OPTIONS_GHC -Wno-overflowed-literals #-}

module Data.Chunk where

import qualified Data.Text                     as T
import           Control.Monad.IO.Class
import           Data.IORef
import           Control.Monad
import           Data.Bits
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           Data.NBT
import           Data.Time.Clock.POSIX          ( POSIXTime )
import           Data.Word
import           Data.Int
import qualified Data.Vector.Unboxed.Mutable   as MVector
import           Data.NBT.Lens
import           Control.Lens
import           Data.Array.Unboxed
import Data.Key
import Data.BlockPalette
import Data.HashTable.IO as HashTable
import Logging.Contextual.BasicScheme
import GHC.Stack
import Utils

import AppMonad

type HashTable k v = HashTable.BasicHashTable k v

data Chunk = Chunk 
  { chunkTimestamp :: !POSIXTime
  , chunkNbt :: !(IORef NbtContents)
  , chunkBlocks :: !(MVector.IOVector Word16)
  }


newChunk :: POSIXTime -> NbtContents -> App Chunk
newChunk chunkTimestamp contents = do
  [logTrace|making new chunk from nbt|]
  chunkNbt <- liftIO $ newIORef contents
  chunkBlocks <- nbtToChunkBlocks contents 
  return Chunk { chunkTimestamp, chunkNbt, chunkBlocks}

updateChunkBlockNbt :: Chunk -> App ()
updateChunkBlockNbt Chunk {chunkBlocks, chunkNbt} = do
  [logTrace|starting updateChunkBlockNbt|]
  nbt <- liftIO $ readIORef chunkNbt
  [logTrace|nbt for chunk is {nbt}|]

  newSections <- chunkBlocksToNbt chunkBlocks
  [logTrace|newSections = {newSections}|]

  let nbt' = set (compoundName "Level" . compoundName "Sections") newSections nbt
  liftIO $ writeIORef chunkNbt nbt'
  [logTrace|finished updateChunkBlockNbt. Final nbt was {nbt'}|]


-- Private

toChunkMem :: Int -> Int -> Int -> Int
toChunkMem !x !y !z = x + (y * 16) + (z * 16 * 256)

buildPaletteMap :: Array Int32 NbtContents -> Map Int T.Text
buildPaletteMap = Map.fromList . map (over _2 f . over _1 fromIntegral) . assocs 
  where f nbtCont = let Just name = nbtCont ^? compoundName "Name" . lnbtContString in name

nbtToChunkBlocks :: NbtContents -> App (MVector.IOVector Word16)
nbtToChunkBlocks nbt = do
  let Just sections = nbt ^? compoundName "Level" . compoundName "Sections" . lnbtContList 
  blocks <- MVector.new $ 16 * 16 * 256
  forM_ sections $ \section -> do
    let Just localBP = fmap buildPaletteMap $ section ^? compoundName "Palette" . lnbtContList
    let Just sectionY = fmap fromIntegral $ section ^? compoundName "Y" . lnbtContInt8
    let Just blockStates = section ^? compoundName "BlockStates" . lnbtContLongArray
    let decodedBlockStates = decode4BitBlockStates blockStates
    unless (length decodedBlockStates == 4096) $ error "unexpected number of block states"

    forWithKeyM decodedBlockStates $ \i blockState -> do
      let x = i `mod` 16
      let y = sectionY * 16 + ((i `div` 16) `mod` 16)
      let z = i `div` 256
      let Just mcBlockId = Map.lookup blockState localBP
      gBlockId <- getBlockId mcBlockId
      MVector.write blocks (toChunkMem x y z) gBlockId
  return blocks

blockLight :: NbtContents
blockLight = ByteArrayTag $ listToArray $ replicate 2048 0

skylight :: NbtContents
skylight = ByteArrayTag $ listToArray $ replicate 2048 0

chunkBlocksToNbt :: HasCallStack => MVector.IOVector Word16 -> App NbtContents
chunkBlocksToNbt chunkBlocks = do
  sectionsList <- forM [0, 1, 2, 3, 4] $ \i -> do
       [logTrace|converting section {i} to NBT format|]
       let sectionSlice = MVector.slice (i * 4096) 4096 chunkBlocks
       (blocksLocalEncoding, paletteList) <- encodeSectorPalette sectionSlice 

       let blockIdCount = length paletteList
       [logTrace|blockIdCount = {blockIdCount}|]

       let palette = listToArray . map (\name -> CompoundTag [NBT "Name" $ StringTag name]) $ paletteList
       [logTrace|computed palette for section was {palette}|]
    
       blocksLocalPacked <- encode4BitBlockStates blocksLocalEncoding 
       return $ CompoundTag $ [ NBT "BlockStates" $ LongArrayTag blocksLocalPacked
                              , NBT "Palette" $ ListTag palette
                              , NBT "BlockLight" $ blockLight
                              , NBT "SkyLight" $ skylight 
                              , NBT "Y" $ ByteTag (fromIntegral i)]
  return $ ListTag $ listToArray sectionsList 

decode4BitBlockStates :: UArray Int32 Int64 -> [Int]
decode4BitBlockStates theArray =
  let asList = elems theArray
  in map fromIntegral $ join $ flip map asList $ \i64 ->
        [ (i64 .&. 0xF)
        , (i64 .&. 0xF0) `shiftR` 4
        , (i64 .&. 0xF00) `shiftR` 8
        , (i64 .&. 0xF000) `shiftR` 12
        , (i64 .&. 0xF0000) `shiftR` 16
        , (i64 .&. 0xF00000) `shiftR` 20
        , (i64 .&. 0xF000000) `shiftR` 24
        , (i64 .&. 0xF0000000) `shiftR` 28
        , (i64 .&. 0xF00000000) `shiftR` 32
        , (i64 .&. 0xF000000000) `shiftR` 36
        , (i64 .&. 0xF0000000000) `shiftR` 40
        , (i64 .&. 0xF00000000000) `shiftR` 44
        , (i64 .&. 0xF000000000000) `shiftR` 48
        , (i64 .&. 0xF0000000000000) `shiftR` 52
        , (i64 .&. 0xF00000000000000) `shiftR` 56
        , (i64 .&. 0xF000000000000000) `shiftR` 60
        ]

encodeSectorPalette :: MVector.IOVector Word16 -> App (MVector.IOVector Int32, [T.Text])
encodeSectorPalette blocks = do
  [logTrace|Running encodeSectorPalette
    -resolve global block ids with the block palette and produce a list of unique minecraft
     block ids and a buffer of blocks encoded as indexes into that list
  |]

  localBp <- liftIO $ HashTable.new :: App (HashTable T.Text Int32)
  paletteRef <- liftIO $ newIORef []
  nextIdRef <- liftIO $ newIORef 0
  result <- liftIO $ MVector.new (MVector.length blocks)

  [logTrace|blocks lenght {MVector.length blocks}|]

  forM_ [0..MVector.length blocks - 1] $ \i -> do
    gBlockId <- liftIO $ MVector.read blocks i
    mcName <- getMCName gBlockId
    localId <- (liftIO $ HashTable.lookup localBp mcName) >>= \case
      Just localId -> return localId
      Nothing -> do
        nextId <- liftIO $ readIORef nextIdRef
        [logTrace|new local block found: {mcName}. Assigning it id {nextId}|]
        liftIO $ writeIORef nextIdRef (nextId + 1)
        liftIO $ HashTable.insert localBp mcName nextId
        liftIO $ modifyIORef paletteRef (mcName:)
        return nextId
    liftIO $ MVector.write result i localId
  
  palette <- liftIO $ reverse <$> readIORef paletteRef
  [logTrace|Finished encodeSectorPalette. Final local palette was {palette}|]
  return (result, palette)

encode4BitBlockStates :: MVector.IOVector Int32 -> App (UArray Int32 Int64)
encode4BitBlockStates theVector = do
  [logTrace|encoding |]
  let len = MVector.length theVector
  asList <- forM [0,16..len - 1] $ \i -> do
    let thisBit = MVector.slice i 16 theVector
    let andIO a b = (.&.) <$> a <*> b
    let get :: Int -> IO Int64
        get j = do
          v <- MVector.read thisBit j
          return $ fromIntegral v `shiftL` (j * 4)
    liftIO $ 
      get 0 `andIO` 
      get 1 `andIO`
      get 2 `andIO`
      get 3 `andIO`
      get 4 `andIO`
      get 5 `andIO`
      get 6 `andIO`
      get 7 `andIO`
      get 8 `andIO`
      get 9 `andIO`
      get 10 `andIO`
      get 11 `andIO`
      get 12 `andIO`
      get 13 `andIO`
      get 14 `andIO`
      get 15
  return $ listArray (0, (fromIntegral len `div` 16) - 1) asList