{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}

-- The byte operations of text-short 0.1.6.1 on admitted UTF-8.
module Data.Text.Short.Native
  ( c_text_short_length, c_text_short_is_ascii, c_text_short_is_valid_utf8
  , c_text_short_index, c_text_short_index_rev
  , c_text_short_index_ofs, c_text_short_index_ofs_rev
  , c_text_short_ofs_cp, c_text_short_ofs_cp_rev
  ) where

import Data.Bits ((.&.), (.|.), shiftL)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Short.Internal as Short
import qualified Data.Text.Encoding as Text
import Foreign.C.Types (CInt, CSize)
import GHC.Exts (ByteArray#, Int(..), Word(..), indexWord8Array#, word8ToWord#)

c_text_short_length :: ByteArray# -> CSize -> IO CSize
c_text_short_length bytes# count = pure (fromIntegral (go 0 0))
  where
    size = fromIntegral count
    go :: Int -> Int -> Int
    go !offset !total
      | offset >= size = total
      | lead (octet bytes# offset) = go (offset + 1) (total + 1)
      | otherwise = go (offset + 1) total

c_text_short_is_ascii :: ByteArray# -> CSize -> IO CInt
c_text_short_is_ascii bytes# count = pure (go 0)
  where
    size = fromIntegral count
    go !offset
      | offset >= size = 1
      | octet bytes# offset >= 0x80 = 0
      | otherwise = go (offset + 1)

c_text_short_is_valid_utf8 :: ByteArray# -> CSize -> IO CInt
c_text_short_is_valid_utf8 bytes# count =
  case Text.decodeUtf8' (BS.take (fromIntegral count) (Short.fromShort (Short.SBS bytes#))) of
    Left _ -> pure 1
    Right _ -> pure 0

c_text_short_index :: ByteArray# -> CSize -> CSize -> IO Word
c_text_short_index bytes# count index =
  atOffset bytes# count <$> c_text_short_index_ofs bytes# count index

c_text_short_index_rev :: ByteArray# -> CSize -> CSize -> IO Word
c_text_short_index_rev bytes# count index =
  atOffset bytes# count <$> c_text_short_index_ofs_rev bytes# count index

atOffset :: ByteArray# -> CSize -> CSize -> Word
atOffset bytes# count offset
  | offset >= count = maxBound
  | otherwise = codePoint bytes# (fromIntegral offset)

c_text_short_index_ofs :: ByteArray# -> CSize -> CSize -> IO CSize
c_text_short_index_ofs bytes# count index = pure (fromIntegral (go 0 index))
  where
    size = fromIntegral count
    go !offset !remaining
      | offset >= size || remaining == 0 = min offset size
      | otherwise = go (offset + width (octet bytes# offset)) (remaining - 1)

c_text_short_index_ofs_rev :: ByteArray# -> CSize -> CSize -> IO CSize
c_text_short_index_ofs_rev bytes# count index = pure (fromIntegral (go size index))
  where
    size = fromIntegral count
    go !end !remaining
      | end <= 0 = size
      | remaining == 0 = previousLead bytes# end
      | otherwise = go (previousLead bytes# end) (remaining - 1)

c_text_short_ofs_cp :: ByteArray# -> CSize -> IO Word
c_text_short_ofs_cp bytes# offset = pure (codePoint bytes# (fromIntegral offset))

c_text_short_ofs_cp_rev :: ByteArray# -> CSize -> IO Word
c_text_short_ofs_cp_rev bytes# offset =
  pure (codePoint bytes# (previousLead bytes# (fromIntegral offset)))

previousLead :: ByteArray# -> Int -> Int
previousLead bytes# = seek . subtract 1
  where
    seek offset
      | offset <= 0 || lead (octet bytes# offset) = max 0 offset
      | otherwise = seek (offset - 1)

codePoint :: ByteArray# -> Int -> Word
codePoint bytes# offset = case width first of
  1 -> first
  2 -> append (first .&. 0x1f) 1
  3 -> append (append (first .&. 0x0f) 1) 2
  _ -> append (append (append (first .&. 0x07) 1) 2) 3
  where
    first = octet bytes# offset
    append value byte = shiftL value 6 .|. (octet bytes# (offset + byte) .&. 0x3f)

width :: Word -> Int
width first
  | first < 0x80 = 1
  | first < 0xe0 = 2
  | first < 0xf0 = 3
  | otherwise = 4

lead :: Word -> Bool
lead value = value .&. 0xc0 /= 0x80

octet :: ByteArray# -> Int -> Word
octet bytes# (I# offset#) = W# (word8ToWord# (indexWord8Array# bytes# offset#))
