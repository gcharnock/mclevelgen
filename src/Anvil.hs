{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module Anvil where

import Control.Monad.IO.Class
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
import           Data.NBT                       ( NBT )
import           Data.Serialize                 ( Serialize(..)
                                                , Get
                                                , Put
                                                , decodeLazy
                                                , encodeLazy
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
import           Pipes                          ( Pipe
                                                , runEffect
                                                , (>->)
                                                , (<-<)
                                                , await
                                                , each
                                                , yield
                                                )
import           System.IO                      ( Handle
                                                , hSeek
                                                , hTell
                                                , SeekMode(AbsoluteSeek)
                                                )

import Numeric
import Data.Region
import Utils


data CompressionType
  = GZip -- ^ unused in practice
  | Zlib
  deriving (Eq, Ord, Read, Show, Data, Typeable, Generic)

-- | ChunkData
--
-- Compressed chunk data
data ZippedChunkData = ZippedChunkData
  { chunkDataLength      :: !Word32 -- ^ length of chunkData + 1
  , chunkDataCompression :: !CompressionType -- ^ compression type
  , chunkData            :: !ByteString -- ^ compressed data (length - 1)
  }
  deriving (Eq, Ord, Read, Show, Data, Typeable, Generic)


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
encodeChunkLocation offset length = (offset .&. 0xFFFFFF) .|. ((length .&.  0xFF) `shiftL` 24)

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
visualiseChunks AnvilHeader { locations } = unlines lines
 where
  lines =
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

-- | make an 'AnvilHeader'
--
-- assumes the chunks will be written in the same order as they appear in the list
-- mkAnvilHeader :: Region -> AnvilHeader
-- mkAnvilHeader Region { regionChunkMap } =
--   let timestamps' = map (\(i, (_, t)) -> (chunkIndex i, t)) regionChunkMap
--       locations'  = snd $ mapAccumL mkLocation 0x2 chunks -- ^ first chunk is at sector 0x2, after the AnvilHeader
--   in  AnvilHeader
--         { locations  = (locations emptyAnvilHeader) Vector.// locations'
--         , timestamps = (timestamps emptyAnvilHeader) Vector.// timestamps'
--         }
--  where
--   mkLocation
--     :: Word24 -> ((ChunkX, ChunkZ), (ZippedChunkData, POSIXTime)) -> (Word24, (Int, ChunkLocation))
--   mkLocation offset (chunkPos, (chunkData, _)) =
--     let paddedSectorLength =
--           ((chunkDataLength chunkData) + 4 + 4095) `div` 4096
--         offset' = offset + paddedSectorLength
--     in  ( offset'
--         , ( chunkIndex chunkPos
--           , ChunkLocation offset (fromIntegral paddedSectorLength)
--           )
--         )
-- 


putChunkData :: ZippedChunkData -> Put
putChunkData cd = do
  putWord32be (chunkDataLength cd)
  case chunkDataCompression cd of
    GZip -> putWord8 1
    Zlib -> putWord8 2
  putLazyByteString (chunkData cd)

getChunkData :: Get ZippedChunkData
getChunkData = do
  len  <- getWord32be
  comp <- do
    w <- getWord8
    case w of
      1 -> pure GZip
      2 -> pure Zlib
      _ ->
        error $ "Unknown compression code in getChunkData: " ++ show (len, w)
  bs <- getLazyByteString (fromIntegral (len - 1))
  pure $ ZippedChunkData len comp bs

instance Serialize ZippedChunkData where
  put = putChunkData
  get = getChunkData

-- | read file header form a Seekable Handle
readHeader :: Handle -> IO AnvilHeader
readHeader h = do
  hSeek h AbsoluteSeek 0
  (r, _) <- runStateT (decodeGet getAnvilHeader) (fromHandle h)
  case r of
    Left  err    -> error $ "could not read header" ++ err
    Right header -> return header


-- | read 'ChunkData' from a Seekable 'Handle'
readChunkData :: Handle -> ChunkLocation -> IO (Maybe ZippedChunkData)
readChunkData h chunkLocation
  | chunkLocation == emptyChunkLocation = pure Nothing
  | otherwise = do
    hSeek h AbsoluteSeek (fromIntegral $ ((chunkOffset chunkLocation) * 4096))
    (r, _) <- runStateT (decodeGet getChunkData) (fromHandle h)
    case r of
      (Left  err) -> error err
      (Right cd ) -> pure (Just cd)

writeChunkData :: (MonadIO m) => ZippedChunkData -> Pipes.Producer B.ByteString m ()
writeChunkData zippedChunkData = do
  encodePut (putChunkData zippedChunkData)
  let padding = 4096 - ((chunkDataLength zippedChunkData + 4) `mod` 4096)
  when (padding > 0) (yield (B.replicate (fromIntegral padding) 0))

decompressChunkData :: ZippedChunkData -> Either String NBT
decompressChunkData cd
  | chunkDataCompression cd == Zlib = decodeLazy (decompress (chunkData cd))
  | otherwise = error $ "decompressChunkData not implemented for " ++ show
    (chunkDataCompression cd)

-- | NBT needs to be a Chunk
compressChunkData :: Chunk -> ZippedChunkData
compressChunkData Chunk {chunkNbt} =
  let d = compress (encodeLazy chunkNbt)
  in  ZippedChunkData
        { chunkDataLength      = 1 + fromIntegral (LB.length d)
        , chunkDataCompression = Zlib
        , chunkData            = d
        }

chunkCoordsToHeaderLoc :: (ChunkX, ChunkZ) -> Int
chunkCoordsToHeaderLoc (chunkX, chunkZ) = chunkX + chunkZ * 32

putVecW32 :: Vector Word32 -> Put
putVecW32 = Vector.mapM_ putWord32be

writeRegion :: Handle -> Region -> IO ()
writeRegion h Region { regionChunkMap } = do
  let chunks = Map.toAscList regionChunkMap
  locationBuff <- liftIO $ MVector.new 1024
  liftIO $ MVector.set locationBuff emptyChunkLocation
  timestampBuff <- liftIO $ MVector.new 1024
  liftIO $ MVector.set timestampBuff 0


  let byteProducer :: Pipes.Producer B.ByteString IO ()
      byteProducer = Pipes.for (each chunks) $ \(coords, chunk) -> do
        let zippedChunkData = compressChunkData chunk
        let dataLen = chunkDataLength zippedChunkData
        offset <- liftIO $ hTell h
        unless (offset `mod` 4096 == 0) $ error $ "offset should be a multiple of 4096 but was " <> show offset
        writeChunkData zippedChunkData
        let offsetSectors = offset `div` 4096
        let lengthSector = if dataLen `mod` 4096 == 0 then dataLen `div` 4096 else dataLen `div` 4096 + 1
        let headerLoc = chunkCoordsToHeaderLoc coords
        let chunkLoc = ChunkLocation { chunkOffset = fromIntegral offsetSectors
                                     , chunkLength = fromIntegral lengthSector }
        liftIO $ putStrLn $ "Wrote Chunk..." <> show coords <> " " <> (show dataLen) <> "b" <> 
                 " headerLoc " <> (show headerLoc) <>
                 " offset=" <> show offsetSectors <>
                 " len=" <> show lengthSector <>
                 " encoded=" <> show chunkLoc
        liftIO $ MVector.write locationBuff headerLoc chunkLoc 

  hSeek h AbsoluteSeek 8192
  runEffect $ toHandle h <-< byteProducer

  hSeek h AbsoluteSeek 0
  locations <- Vector.freeze locationBuff
  runEffect $ toHandle h <-< (Pipes.for (each locations) $ \loc -> (encodePut $ putChunkLocation loc))
  
  pos <- liftIO $ hTell h
  unless (pos == 4096) $ error $ "expected pos=4096 but was " <> show pos
  
  -- write timestamps
  timestamps <- Vector.freeze timestampBuff
  runEffect $ encodePut (putVecW32 timestamps) >-> toHandle h
  --let anvilHeaderProducer = encodePut $ putAnvilHeader header
  --runEffect $ anvilHeaderProducer >-> consumer
  pure ()

readRegion :: Handle -> IO Region
readRegion h = do
  chunkMap <- newIORef Map.empty
  hSeek h AbsoluteSeek 0
  let producer = fromHandle h
  (Right header, leftover) <- runEffect
    $ runStateT (decodeGet getAnvilHeader) producer
  let locs = locations header
  iforM_ locs $ \i ChunkLocation {chunkOffset, chunkLength} -> do
    (Right chunk, _) <- runEffect $ runStateT (decodeGet getChunkData) leftover
    let chunkX = i `mod` 32
    let chunkZ = i `div` 32
    modifyIORef chunkMap $ Map.insert (chunkX, chunkZ) (chunk, 0)
  chunkMap' <- readIORef chunkMap
  let chunkMap = flip Map.map chunkMap' $ \(chunkData, _) ->
                  case decompressChunkData chunkData of
                    Right nbt -> Chunk { chunkNbt = nbt, chunkTimestamp = 0 }
  return Region { regionChunkMap = chunkMap }

