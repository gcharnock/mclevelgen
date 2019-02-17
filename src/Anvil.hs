{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Anvil where

import Control.Monad.IO.Class
import           Control.Monad
import           Data.Bits                      ( shiftR
                                                , shiftL
                                                , (.&.)
                                                , (.|.)
                                                )
import qualified Data.ByteString               as B
import qualified Data.ByteString.Lazy          as LB
import           Data.Data                      ( Data
                                                , Typeable
                                                )
import qualified Data.Map                      as Map
import           Data.NBT
import           Data.Serialize                 ( Serialize(..)
                                                , Get
                                                , Put
                                                , getLazyByteString
                                                , getWord8
                                                , getWord32be
                                                , putLazyByteString
                                                , putWord8
                                                , putWord32be
                                                )
import           Data.Time.Clock.POSIX          ( POSIXTime )
import           Data.Vector                    ( Vector, (!) )
import qualified Data.Vector                   as Vector
import qualified Data.Vector.Mutable           as MVector
import           Data.Word                      ( Word8
                                                , Word32
                                                )
import           GHC.Generics
import           Pipes.ByteString               ( fromHandle
                                                , toHandle
                                                )
import           Pipes.Cereal                   ( decodeGet
                                                , encodePut
                                                )
import           Pipes.Parse                    ( runStateT )
import qualified Pipes
import           Pipes                          ( runEffect
                                                , (>->)
                                                , (<-<)
                                                , each
                                                , yield
                                                )
import           System.IO                      ( Handle
                                                , SeekMode(AbsoluteSeek)
                                                )
import Data.Region
import Utils
import Control.Monad.Reader.Class
import Logging.Contextual
import Logging.Contextual.BasicScheme
import Control.Monad.Trans.Class
import UnliftIO.IORef
import UnliftIO.IO
import Data.ZippedChunk


import AppMonad


type Word24 = Word32

-- | ChunkLocation
--
-- ChunkLocation 0 0 means chunk is not present in the file
data ChunkLocation = ChunkLocation
  { chunkOffset :: !Word24 -- ^ number of 4KiB sectors from start of file
  , chunkLength :: !Word8 -- ^ length of chunk -- units are 4KiB sectors, rounded up
  }
  deriving (Eq, Ord, Read, Show, Data, Typeable, Generic)

-- | ChunkLocation 0 0 means chunk is not present in the file
emptyChunkLocation :: ChunkLocation
emptyChunkLocation = ChunkLocation 0 0

putChunkLocation :: ChunkLocation -> Put
putChunkLocation ChunkLocation { chunkOffset = offset, chunkLength = len } = do
  putWord8 $ fromIntegral $ (offset `shiftR` 16) .&. 0xFF
  putWord8 $ fromIntegral $ (offset `shiftR` 8) .&. 0xFF
  putWord8 $ fromIntegral $ offset .&. 0xFF
  putWord8 len

encodeChunkLocation :: Word32 -> Word32 -> Word32
encodeChunkLocation offset len = (offset .&. 0xFFFFFF) .|. ((len .&.  0xFF) `shiftL` 24)

getChunkLocation :: Get ChunkLocation
getChunkLocation = do
  b2  <- fromIntegral <$> getWord8
  b1  <- fromIntegral <$> getWord8
  b0  <- fromIntegral <$> getWord8
  len <- fromIntegral <$> getWord8
  pure (ChunkLocation ((b2 `shiftL` 16) .|. (b1 `shiftL` 8) .|. b0) len)

instance Serialize ChunkLocation where
  put = putChunkLocation
  get = getChunkLocation

data AnvilHeader = AnvilHeader
  { locations  :: Vector ChunkLocation
  , timestamps :: Vector POSIXTime
  }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)


showAnvilHeader :: AnvilHeader -> String
showAnvilHeader ah = show $ AnvilHeader
  { locations  = Vector.filter (/= emptyChunkLocation) (locations ah)
  , timestamps = Vector.filter (/= 0) (timestamps ah)
  }
-- The location in the region file of a chunk at (x, z) (in chunk coordinates) can be found at byte offset 4 * ((x mod 32) + (z mod 32) * 32)


visualiseChunks :: AnvilHeader -> String
visualiseChunks AnvilHeader { locations } = unlines lines'
 where
  lines' =
    [ map
        (\x -> if emptyChunkLocation /= (locations ! (32 * z + x))
          then '*'
          else ' '
        )
        [0 .. 31]
    | z <- [0 .. 31]
    ]

-- guessing on the timestamp format a bit
putTimestamp :: POSIXTime -> Put
putTimestamp t = putWord32be (round t)

getTimestamp :: Get POSIXTime
getTimestamp = do
  l <- getWord32be
  pure $ (realToFrac l)

getAnvilHeader :: Get AnvilHeader
getAnvilHeader = do
  locations  <- replicateM 1024 getChunkLocation
  timestamps <- replicateM 1024 getTimestamp
  pure $ AnvilHeader (Vector.fromList locations) (Vector.fromList timestamps)

emptyAnvilHeader :: AnvilHeader
emptyAnvilHeader = AnvilHeader
  { locations  = Vector.replicate 1024 emptyChunkLocation
  , timestamps = Vector.replicate 1024 0
  }

putZippedChunk :: ZippedChunk -> Put
putZippedChunk cd = do
  putWord32be (zippedChunkLength cd)
  case zippedChunkCompression cd of
    GZip -> putWord8 1
    Zlib -> putWord8 2
  putLazyByteString (zippedChunkBytes cd)

getZippedChunk :: Get ZippedChunk
getZippedChunk = do
  len  <- getWord32be
  comp <- do
    w <- getWord8
    case w of
      1 -> pure GZip
      2 -> pure Zlib
      _ ->
        error $ "Unknown compression code in getChunkData: " ++ show (len, w)
  bs <- getLazyByteString (fromIntegral (len - 1))
  pure $ ZippedChunk len comp bs


-- | read file header form a Seekable Handle
readHeader :: Handle -> IO AnvilHeader
readHeader h = do
  hSeek h AbsoluteSeek 0
  (r, _) <- runStateT (decodeGet getAnvilHeader) (fromHandle h)
  case r of
    Left  err    -> error $ "could not read header" ++ err
    Right header -> return header


-- | read 'ChunkData' from a Seekable 'Handle'
readChunkData :: Handle -> ChunkLocation -> IO (Maybe ZippedChunk)
readChunkData h chunkLocation
  | chunkLocation == emptyChunkLocation = pure Nothing
  | otherwise = do
    hSeek h AbsoluteSeek (fromIntegral $ ((chunkOffset chunkLocation) * 4096))
    (r, _) <- runStateT (decodeGet getZippedChunk) (fromHandle h)
    case r of
      (Left  err) -> error err
      (Right cd ) -> pure (Just cd)

writeChunkData :: (MonadIO m, HasLog env, MonadReader env m)
               => ZippedChunk -> Pipes.Producer B.ByteString m ()
writeChunkData zippedChunk@ZippedChunk {zippedChunkLength} = do
  [logTrace|writeChunkData - writing zipped chunk data to producer. length = {zippedChunkLength}}|]
  encodePut (putZippedChunk zippedChunk)
  
  let padding = 4096 - ((zippedChunkLength + 4) `mod` 4096)

  [logTrace|padding = {padding}|]
  when (padding > 0) (yield (B.replicate (fromIntegral padding) 0))



chunkCoordsToHeaderLoc :: (ChunkX, ChunkZ) -> Int
chunkCoordsToHeaderLoc (chunkX, chunkZ) = chunkX + chunkZ * 32

putVecW32 :: Vector Word32 -> Put
putVecW32 = Vector.mapM_ putWord32be

writeRegion :: Handle -> Region -> App ()
writeRegion h Region { regionChunkMap } = do
  [logInfo|writeRegion: writing a region to disk|]
  let chunks = Map.toAscList regionChunkMap

  locationBuff <- liftIO $ MVector.new 1024
  liftIO $ MVector.set locationBuff emptyChunkLocation
  timestampBuff <- liftIO $ MVector.new 1024
  liftIO $ MVector.set timestampBuff 0

  [logTrace|allocated buffers for writing the region header|]

  let byteProducer :: Pipes.Producer B.ByteString App ()
      byteProducer = do
        [logTrace|running producer for chunks|]

        Pipes.for (each chunks) $ \(coords, zippedChunk) -> do
          [logTrace|running producer over chunk with coords {coords}|]

          let dataLen = zippedChunkLength zippedChunk

          offset <- liftIO $ hTell h
          let offsetSectors = offset `div` 4096

          [logTrace|offset in the file was {offset} or {offsetSectors} sectors|]
          unless (offset `mod` 4096 == 0) $ do
            [logError|offset should be a multiple of 4096 but was {offset}|]
            error $ "offset should be a multiple of 4096 but was " <> show offset

          writeChunkData zippedChunk
          let lengthSector = 
                if dataLen `mod` 4096 == 0
                  then dataLen `div` 4096
                  else dataLen `div` 4096 + 1
          [logTrace|lengthSector = {lengthSector}|]

          let headerLoc = chunkCoordsToHeaderLoc coords
          [logTrace|coords {coords} ==> headerLoc = {headerLoc}|]

          let chunkLoc = ChunkLocation { chunkOffset = fromIntegral offsetSectors
                                       , chunkLength = fromIntegral lengthSector }
          [logTrace|chunkLoc = {chunkLoc}|]

          [logTrace|Wrote Chunk... {coords} {dataLen}b {headerLoc} {headerLoc} offset={offsetSectors}
                   len={lengthSector} encoded={chunkLoc}|]
          liftIO $ MVector.write locationBuff headerLoc chunkLoc 

  [logTrace|hSeak file handle to start of chunks|]
  liftIO $ hSeek h AbsoluteSeek 8192
  runEffect $ toHandle h <-< byteProducer

  [logTrace|hSeak file handle to zero|]
  liftIO $ hSeek h AbsoluteSeek 0

  [logTrace|freezing location buffer|]
  locations <- Vector.freeze locationBuff
  runEffect $ toHandle h <-< (Pipes.for (each locations) $ \loc -> (encodePut $ putChunkLocation loc))
  
  pos <- liftIO $ hTell h
  unless (pos == 4096) $ do
    [logError|expected pos=4096 but was {pos}|]
    error $ "expected pos=4096 but was " <> show pos
  
  -- write timestamps
  timestamps <- Vector.freeze timestampBuff
  runEffect $ encodePut (putVecW32 timestamps) >-> toHandle h
  --let anvilHeaderProducer = encodePut $ putAnvilHeader header
  --runEffect $ anvilHeaderProducer >-> consumer
  pure ()

readRegion :: Handle -> App Region
readRegion h = do
  [logInfo|starting readRegion|]

  [logTrace|createing empty chunk map|]
  chunkMapRef <- newIORef Map.empty
  hSeek h AbsoluteSeek 0

  let producer = fromHandle h
  (Right header, leftover) <- runEffect $ runStateT (decodeGet getAnvilHeader) producer
  let locs = locations header

  [logInfo|reading all chunk locations|]
  iforM_ locs $ \i ChunkLocation {} -> do
    -- TODO: I think we might be reading the same chunk over and over here
    -- as we're not using the chunk location.
    [logTrace|loading chunk described at header location {i}|]
    (Right chunk, _) <- runEffect $ runStateT (decodeGet getZippedChunk) leftover
    let chunkX = i `mod` 32
    let chunkZ = i `div` 32
    [logTrace|inserting chunk ({chunkX}, {chunkZ}) into chunk map|]
    modifyIORef chunkMapRef $ Map.insert (chunkX, chunkZ) chunk

  chunkMap <- readIORef chunkMapRef
  return Region { regionChunkMap = chunkMap }

