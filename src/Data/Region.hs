
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -Wno-overflowed-literals #-}

module Data.Region where

import qualified Data.Text                     as T
import           Data.Map                       ( Map )
import qualified Data.Map                       as Map
import Data.Bits
import Data.ZippedChunk
import Data.HashTable.IO as HashTable
import Data.Chunk
import Data.ZippedChunk
import Logging.Contextual.BasicScheme

import AppMonad

type ChunkX = Int
type ChunkZ = Int
type RegionX = Int
type RegionZ = Int


data Region = Region 
  { regionChunkMap :: !(Map (ChunkX, ChunkZ) ZippedChunk)
  }

data RegionCoord = RegionCoord {
  regionCoordX :: !Int,
  regionCoordY :: !Int,
  regionCoordZ :: !Int
} deriving (Show)

-- Private


regionX :: ChunkX -> RegionX
regionX x = x `shiftR` 5

regionZ :: ChunkZ -> RegionZ
regionZ z = z `shiftR` 5

chunkIndex :: (ChunkX, ChunkZ) -> Int
chunkIndex (x, z) = (x `pmod` 32) + ((z `pmod` 32) * 32)
  where pmod n m = let n' = n `mod` m in
                    if n' >= 0 then n' else n' + m

loadChunk :: Region -> (ChunkX, ChunkZ) -> App Chunk
loadChunk Region { regionChunkMap } coords = do
  [logTrace|loading chunk at {coords}|]
  let Just zippedChunk = Map.lookup coords regionChunkMap
  decompressChunk zippedChunk
  
 
saveChunk :: Region -> (ChunkX, ChunkZ) -> Chunk -> App ()
saveChunk Region { regionChunkMap } coords chunk = do
  zippedChunk <- compressChunk chunk
  let dataLen = zippedChunkLength zippedChunk
  [logTrace|length of compressed data was {dataLen}|]
