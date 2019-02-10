{-# OPTIONS_GHC -Wno-overflowed-literals #-}

module Data.Chunk where

import qualified Data.Text                     as T
import           Control.Monad.IO.Class
import           Data.IORef
import           Control.Monad
import           Control.Monad.State.Strict     ( execStateT
                                                , modify'
                                                )
import           Codec.Compression.Zlib         ( decompress
                                                , compress
                                                )
import           Data.Bits                      ( shiftR
                                                , shiftL
                                                , (.&.)
                                                , (.|.)
                                                )
import           Data.ByteString.Lazy           ( ByteString )
import qualified Data.ByteString               as B
import qualified Data.ByteString.Lazy          as LB
import           Data.Data                      ( Data
                                                , Typeable
                                                )
import           Data.List                      ( mapAccumL )
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           Data.NBT
import           Data.Time.Clock.POSIX          ( POSIXTime )
import           Data.Vector                    ( Vector )
import qualified Data.Vector                   as Vector
import           Data.Word
import           Data.Int
import qualified Data.Vector.Unboxed.Mutable   as MVector
import qualified Data.Array.Repa               as Repa
import           Data.NBT.Lens
import           Control.Lens
import           Control.Monad.ST
import           Data.Array.Unboxed
import           Data.Foldable
import Data.Key
import Data.BlockPalette
import Data.HashTable.IO as HashTable
import Logging.Contextual.BasicScheme

import AppMonad

type HashTable k v = HashTable.BasicHashTable k v

data Chunk = Chunk 
  { chunkTimestamp :: !POSIXTime
  , chunkNbt :: !(IORef NbtContents)
  , chunkBlocks :: !(MVector.IOVector Word16)
  }


newChunk :: BlockPalette -> POSIXTime -> NbtContents -> App Chunk
newChunk bp chunkTimestamp contents = do
  [logTrace|making new chunk from nbt|]
  chunkNbt <- liftIO $ newIORef contents
  chunkBlocks <- nbtToChunkBlocks bp contents 
  return Chunk { chunkTimestamp, chunkNbt, chunkBlocks}

updateChunkBlockNbt :: BlockPalette -> Chunk -> App ()
updateChunkBlockNbt bp Chunk {chunkBlocks, chunkNbt} = do
  [logTrace|starting updateChunkBlockNbt|]
  nbt <- liftIO $ readIORef chunkNbt
  newSections <- chunkBlocksToNbt bp chunkBlocks
  let nbt' = set (compoundName "Level" . compoundName "Sections") newSections nbt
  liftIO $ writeIORef chunkNbt nbt'
  [logTrace|finished updateChunkBlockNbt|]


-- Private

toChunkMem :: Int -> Int -> Int -> Int
toChunkMem !x !y !z = x + (y * 16) + (z * 16 * 256)

buildPaletteMap :: Array Int32 NbtContents -> Map Int T.Text
buildPaletteMap = Map.fromList . map (over _2 f . over _1 fromIntegral) . assocs 
  where f nbtCont = let Just name = nbtCont ^? compoundName "Name" . lnbtContString in name

nbtToChunkBlocks :: BlockPalette -> NbtContents -> App (MVector.IOVector Word16)
nbtToChunkBlocks bp nbt = do
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
      gBlockId <- getBlockId bp mcBlockId
      MVector.write blocks (toChunkMem x y z) gBlockId
  return blocks

chunkBlocksToNbt :: BlockPalette -> MVector.IOVector Word16 -> App NbtContents
chunkBlocksToNbt bp chunkBlocks = do
  sectionsList <- forM [0, 1, 2, 3, 4] $ \i -> do
       [logTrace|converting section {i} to NBT format|]
       let sectionSlice = MVector.slice (i * 4096) 4096 chunkBlocks
       (blocksLocalEncoding, paletteList) <- encodeSectorPalette bp sectionSlice 

       let palette = listArray (0, fromIntegral $ length paletteList) . 
                     map (\name -> CompoundTag [NBT "Name" $ StringTag name]) $ paletteList
       [logTrace|computed palette for section was {palette}|]
    
       blocksLocalPacked <- encode4BitBlockStates blocksLocalEncoding 
       return $ CompoundTag $ [ NBT "BlockStates" $ LongArrayTag blocksLocalPacked
                              , NBT "Palette" $ ListTag palette]
  return $ ListTag $ listArray (0, fromIntegral $ length sectionsList) sectionsList 

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

encodeSectorPalette :: BlockPalette -> MVector.IOVector Word16 -> App (MVector.IOVector Int32, [T.Text])
encodeSectorPalette bp blocks = do
  [logTrace|Running encodeSectorPalette|]
  localBp <- liftIO $ HashTable.new :: App (HashTable T.Text Int32)
  paletteRef <- liftIO $ newIORef []
  nextIdRef <- liftIO $ newIORef 0
  result <- liftIO $ MVector.new (MVector.length blocks)

  [logTrace|blocks lenght {MVector.length blocks}|]

  forM_ [0..MVector.length blocks - 1] $ \i -> do
    gBlockId <- liftIO $ MVector.read blocks i
    mcName <- getMCName bp gBlockId
    localId <- (liftIO $ HashTable.lookup localBp mcName) >>= \case
      Just localId -> return localId
      Nothing -> do
        nextId <- liftIO $ readIORef nextIdRef
        liftIO $ writeIORef nextIdRef (nextId + 1)
        liftIO $ HashTable.insert localBp mcName nextId
        liftIO $ modifyIORef paletteRef (mcName:)
        return nextId
    liftIO $ MVector.write result i localId
  
  palette <- liftIO $ reverse <$> readIORef paletteRef
  [logTrace|Finished encodeSectorPalette|]
  return (result, palette)

encode4BitBlockStates :: MVector.IOVector Int32 -> App (UArray Int32 Int64)
encode4BitBlockStates theVector = do
  let len = MVector.length theVector
  asList <- forM [0,16..len - 1] $ \i -> do
    [logTrace|slice {i} , {i+15} from vector of length {len}|]

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
  return $ listArray (0, fromIntegral len `div` 16) asList