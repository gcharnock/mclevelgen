{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

import           Data.Int
import           Data.Array
import           Control.Monad
import           Control.Lens
import qualified Codec.Compression.GZip        as GZip
import qualified Codec.Compression.Zlib        as Zlib
import qualified Data.ByteString.Lazy          as BL
import           Data.NBT
import           Data.NBT.Lens
import           Data.Serialize
import qualified Data.Map                      as Map
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T
import           System.IO
import qualified Data.Array.Unboxed as UArray
import qualified Data.Time.Clock.POSIX as POSIX

import           Anvil
import           Data.Region

showT :: Show a => a -> T.Text
showT = T.pack . show

dumpChunk :: Handle -> IO ()
dumpChunk handle = readChunkData handle (ChunkLocation 166 1) >>= \case
    Nothing -> error "readChucnkData"
    Just ZippedChunkData { chunkDataLength, chunkDataCompression, chunkData } -> do
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
changeSection nbtContents = do
    nbtContents' <- forOf (compoundName "Palette" . lnbtContList . each . compoundName "Name" . lnbtContString) nbtContents $ \blockName ->
        if blockName == "minecraft:sandstone"
            then return "minecraft:cobblestone"
            else return blockName
    let blockStates = nbtContents' ^.. compoundName "BlockStates" . lnbtContLongArray
    forM_ blockStates $ \blockState -> do
        T.putStrLn $ "blockStatesLength = " <> (showT $ UArray.bounds blockState)
        T.putStrLn $ "first few  entires = " <> (foldl (\a b -> a <> ", " <> b) "" $ map (showT . (blockState UArray.!)) [0, 1, 2, 3, 4])
    forOf (compoundName "Y") nbtContents f
    let palette = fmap (head . (^.. compoundName "Name" . lnbtContString)) $ head $ nbtContents' ^.. compoundName "Palette" . lnbtContList
    print palette
    return nbtContents'
  where
    f (ByteTag y) = print y >> (return $ ByteTag y)
    f tag         = return tag

changeSections :: NbtContents -> IO NbtContents
changeSections nbtContents =
    forOf (lnbtContList . each) nbtContents changeSection

changeChunk' :: Chunk -> IO Chunk
changeChunk' Chunk { chunkNbt = nbt } = do
    newNbt <- forOf (lnbtContents . compoundName "Level") nbt withLevel
    now <- POSIX.getPOSIXTime
    return Chunk { chunkNbt = newNbt, chunkTimestamp = now }
  where
    withLevel :: NbtContents -> IO NbtContents
    withLevel contents = forOf (compoundName "Sections") contents f
    f :: NbtContents -> IO NbtContents
    f = changeSections

main :: IO ()
main = do
    region <- withFile "example/region/r.-1.-1.mca.old" ReadMode
        $ \handle -> readRegion handle
    --newChunkMap <- mapM changeChunk' (regionChunkMap region)
    withFile "example/region/r.-1.-1.mca" WriteMode
        $ \handle -> writeRegion handle region -- Region { regionChunkMap = newChunkMap }
