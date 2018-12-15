{-# LANGUAGE Rank2Types #-}

module Data.NBT.Lens where

import           Data.Traversable
import           Control.Monad
import           Data.Array.Unboxed
import           Data.Int
import           Data.Text                     as T
import           Control.Lens
import           Data.NBT

lNbtLabel :: Lens' NBT T.Text
lNbtLabel = lens get set
  where
    get (NBT label _) = label
    set (NBT _ contents) label = NBT label contents

lnbtContents :: Lens' NBT NbtContents
lnbtContents = lens get set
  where
    get (NBT _ contents) = contents
    set (NBT label _) = NBT label

lnbtContInt8 :: Prism' NbtContents Int8
lnbtContInt8 = prism' put get
  where
    put = ByteTag
    get (ByteTag i) = Just i
    get _           = Nothing

lnbtContInt16 :: Prism' NbtContents Int16
lnbtContInt16 = prism' put get
  where
    put = ShortTag
    get (ShortTag i) = Just i
    get _            = Nothing

lnbtContInt32 :: Prism' NbtContents Int32
lnbtContInt32 = prism' put get
  where
    put = IntTag
    get (IntTag i) = Just i
    get _          = Nothing

lnbtContInt64 :: Prism' NbtContents Int64
lnbtContInt64 = prism' put get
  where
    put = LongTag
    get (LongTag i) = Just i
    get _           = Nothing


lnbtContFloat :: Prism' NbtContents Float
lnbtContFloat = prism' put get
  where
    put = FloatTag
    get (FloatTag i) = Just i
    get _            = Nothing

lnbtContDouble :: Prism' NbtContents Double
lnbtContDouble = prism' put get
  where
    put = DoubleTag
    get (DoubleTag i) = Just i
    get _             = Nothing

lnbtContByteArray :: Prism' NbtContents (UArray Int32 Int8)
lnbtContByteArray = prism' put get
  where
    put = ByteArrayTag
    get (ByteArrayTag i) = Just i
    get _                = Nothing

lnbtContString :: Prism' NbtContents Text
lnbtContString = prism' put get
  where
    put = StringTag
    get (StringTag i) = Just i
    get _             = Nothing

lnbtContList :: Prism' NbtContents (Array Int32 NbtContents)
lnbtContList = prism' put get
  where
    put = ListTag
    get (ListTag i) = Just i
    get _           = Nothing

lnbtContCompound :: Prism' NbtContents [NBT]
lnbtContCompound = prism' put get
  where
    put = CompoundTag
    get (CompoundTag i) = Just i
    get _               = Nothing

lnbtContIntArray :: Prism' NbtContents (UArray Int32 Int32)
lnbtContIntArray = prism' put get
  where
    put = IntArrayTag
    get (IntArrayTag i) = Just i
    get _               = Nothing

lnbtContLongArray :: Prism' NbtContents (UArray Int32 Int64)
lnbtContLongArray = prism' put get
  where
    put = LongArrayTag
    get (LongArrayTag i) = Just i
    get _                = Nothing

lnbtContCompoundName :: T.Text -> (forall f. Applicative f => (NbtContents -> f NbtContents) -> [NBT] -> f [NBT])
lnbtContCompoundName name f nbtList =
      for nbtList $ \(NBT name' nbtContent) ->
        if name == name'
            then NBT <$> pure name' <*> f nbtContent
            else pure $ NBT name' nbtContent


compoundName :: T.Text -> (forall f. Applicative f => (NbtContents -> f NbtContents) -> NbtContents -> f NbtContents)
compoundName name = lnbtContCompound . lnbtContCompoundName name
