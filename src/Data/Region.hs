
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -Wno-overflowed-literals #-}

module Data.Region where

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
import Data.Chunk
import Data.HashTable.IO as HashTable

type HashTable k v = HashTable.BasicHashTable k v

type ChunkX = Int
type ChunkZ = Int
type RegionX = Int
type RegionZ = Int

data Region = Region 
  { regionChunkMap :: !(Map (ChunkX, ChunkZ) Chunk)
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

setblock :: T.Text -> RegionCoord -> Region -> Region
setblock = undefined

