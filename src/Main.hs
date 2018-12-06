{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

import qualified  Codec.Compression.GZip as GZip
import qualified  Codec.Compression.Zlib as Zlib 
import qualified  Data.ByteString.Lazy as BL
import            Data.NBT
import            Data.Serialize
import Anvil
import System.IO

dumpChunk :: Handle -> IO () 
dumpChunk handle = readChunkData handle (ChunkLocation 166 1) >>= \case
             Nothing -> error "readChucnkData"
             Just ChunkData {chunkDataLength, chunkDataCompression, chunkData} -> do
                let decompress = case chunkDataCompression of
                                            Zlib -> Zlib.decompress 
                                            GZip -> GZip.decompress

                -- NBT files are GZip'd when stored, decompress it and make it strict
                let raw = BL.toStrict $ decompress chunkData

                -- Use the nbt library's Serialize instance to obtain an NBT type,
                -- provided nothing goes wrong!
                let shouldBeNBT = (decode raw :: Either String NBT)

                -- Did we actually just read an NBT file?
                -- If so, print NBT and then write it back out to file.
                -- Otherwise, show the error.
                case shouldBeNBT of
                    Right nbt   -> print nbt
                    Left err    -> putStrLn err

main :: IO ()
main =
    withFile "example/region/r.-1.-1.mca" ReadMode $ \handle -> do
        header <- readHeader handle
        putStrLn $ visualiseChunks header
