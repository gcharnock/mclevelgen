{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

import Data.Int
import Data.Array
import           Control.Monad
import Control.Lens
import qualified Codec.Compression.GZip        as GZip
import qualified Codec.Compression.Zlib        as Zlib
import qualified Data.ByteString.Lazy          as BL
import           Data.NBT
import           Data.NBT.Lens
import           Data.Serialize
import           Data.Map                      as Map
import           Anvil
import           System.IO

dumpChunk :: Handle -> IO ()
dumpChunk handle = readChunkData handle (ChunkLocation 166 1) >>= \case
    Nothing -> error "readChucnkData"
    Just ChunkData { chunkDataLength, chunkDataCompression, chunkData } -> do
        let decompress = case chunkDataCompression of
                Zlib -> Zlib.decompress
                GZip -> GZip.decompress

        -- NBT files are GZip'd when stored, decompress it and make it strict
        let raw         = BL.toStrict $ decompress chunkData

        -- Use the nbt library's Serialize instance to obtain an NBT type,
        -- provided nothing goes wrong!
        let shouldBeNBT = (decode raw :: Either String NBT)

        -- Did we actually just read an NBT file?
        -- If so, print NBT and then write it back out to file.
        -- Otherwise, show the error.
        case shouldBeNBT of
            Right nbt -> print nbt
            Left  err -> putStrLn err



changeSection :: NbtContents -> IO NbtContents
changeSection nbtContents = forOf (lnbtContCompound . compoundName "Y") nbtContents f
  where
    f (ByteTag y) = print y >> (return $ ByteTag y)
    f tag                   = return tag

changeSections :: NbtContents -> IO NbtContents
changeSections nbtContents = forOf (lnbtContList . each) nbtContents changeSection

changeChunk' :: NBT -> IO NBT
changeChunk' nbt = forOf (lnbtContents . lnbtContCompound . compoundName "Level") nbt withLevel
  where withLevel :: NbtContents -> IO NbtContents
        withLevel contents = forOf (lnbtContCompound . compoundName "Sections") contents f
        f :: NbtContents -> IO NbtContents
        f = changeSections

main :: IO ()
main = do
    region <- withFile "example/region/r.-1.-1.mca.old" ReadMode
        $ \handle -> readRegion handle
    region' <- mapM changeChunk' region
    withFile "example/region/r.-1.-1.mca" WriteMode
        $ \handle -> writeRegion handle region'
