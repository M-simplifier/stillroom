{-# LANGUAGE OverloadedStrings #-}

-- Compile the same client against the pinned upstream and adapted packages.
-- The resulting witness files should be byte-identical.
module Main (main) where

import Control.Monad (forM_)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as ASCII
import Data.ByteString.Lazy qualified as Lazy
import Data.ByteString.Short qualified as SBS
import Data.Hashable (hash, hashWithSalt)
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Maybe (isJust)
import Data.Primitive.ByteArray qualified as Array
import Data.Primitive.Ptr qualified as Pointer
import Data.Primitive.Types (Prim, sizeOf)
import Data.Text qualified as Text
import Data.Text.Short qualified as Short
import Data.Word (Word8, Word16, Word32, Word64)
import Foreign.Marshal.Alloc (allocaBytesAligned)
import Foreign.Marshal.Utils (fillBytes)
import Foreign.Ptr (Ptr, castPtr, intPtrToPtr)
import GHC.Float (castWord32ToFloat, castWord64ToDouble)
import System.Environment (getArgs)
import System.Random.SplitMix (mkSMGen, nextWord64)

main :: IO ()
main = do
  arguments <- getArgs
  path <- case arguments of
    [output] -> pure output
    _ -> fail "Usage: native-dependency-check OUTPUT"
  filled <- sequence
    [ fillCase (0xa1 :: Word8), fillCase (-12 :: Int8), fillCase (0xb2c3 :: Word16), fillCase (-12345 :: Int16)
    , fillCase (0x01020304 :: Word32), fillCase (-1234567 :: Int32), fillCase (0x0102030405060708 :: Word64)
    , fillCase (-12345678901234 :: Int64), fillCase (0 :: Word64), fillCase (-0 :: Float), fillCase (-0 :: Double)
    , fillCase (castWord32ToFloat 0x7fc01234), fillCase (castWord64ToDouble 0x7ff8000000001234)
    , fillCase (1.25 :: Float), fillCase (2.625 :: Double), fillCase '\x10ffff', fillCase (intPtrToPtr 0x123456 :: Ptr ())
    ]
  overlap <- moveCase
  BS.writeFile path (BS.concat (overlap : filled ++ [encodingWitness, valueWitness]))

fillCase :: Prim a => a -> IO BS.ByteString
fillCase value = do
  array <- Array.newByteArray 128
  Array.setByteArray array 0 128 (0x5a :: Word8)
  Array.setByteArray array 3 7 value
  Array.setByteArray array 2 0 value
  arrayBytes <- BS.pack <$> mapM (Array.readByteArray array) [0 .. 127]
  pointerBytes <- allocaBytesAligned 128 16 $ \ptr -> do
    fillBytes ptr 0x5a 128
    Pointer.setPtr (Pointer.advancePtr ptr 3) 7 value
    Pointer.setPtr (Pointer.advancePtr ptr 2) 0 value
    BS.packCStringLen (castPtr ptr, 128)
  if arrayBytes == pointerBytes && sizeOf value * 10 <= 128
    then pure arrayBytes
    else fail "Typed fill differed between array and address"

moveCase :: IO BS.ByteString
moveCase = do
  array <- Array.newByteArray 24
  forM_ [0 .. 23] $ \n -> Array.writeByteArray array n (fromIntegral n :: Word8)
  Array.moveByteArray array 3 array 0 14
  Array.moveByteArray array 1 array 4 11
  BS.pack <$> mapM (Array.readByteArray array) [0 .. 23]

encodingWitness :: BS.ByteString
encodingWitness = BS.pack
  [ if isJust (Short.fromByteString bytes) then 1 else 0
  | bytes <- map BS.singleton [0 .. 255] ++ [BS.pack [a,b] | a <- [0 .. 255], b <- [0 .. 255]] ++ longCases
  ]
  where
    longCases = map BS.pack
      [ [0xe0,0x9f,0x80], [0xe0,0xa0,0x80], [0xed,0x9f,0xbf], [0xed,0xa0,0x80], [0xed,0xbf,0xbf]
      , [0xee,0x80,0x80], [0xef,0xbb,0xbf], [0xf0,0x8f,0xbf,0xbf], [0xf0,0x90,0x80,0x80]
      , [0xf4,0x8f,0xbf,0xbf], [0xf4,0x90,0x80,0x80], [0xf0,0x9f,0x8c], [0xff,0xff,0xff,0xff]
      ]

valueWitness :: BS.ByteString
valueWitness = ASCII.pack $ unlines
  [ show [(Short.length s, Short.isAscii s, Short.unpack s, Short.uncons s, Short.unsnoc s) | s <- strings]
  , show [(Short.indexMaybe s n, Short.indexEndMaybe s n, Short.splitAt n s, Short.splitAtEnd n s)
         | s <- strings, n <- [-1 .. Short.length s + 1]]
  , show [(hash b, hashWithSalt seed b, hashWithSalt seed (SBS.toShort b), hashWithSalt seed (lazyChunks b))
         | n <- [0,1,3,7,8,15,16,17,31,63,64,127,128,129,239,240,241,1024,4096,16384],
           let b = BS.pack [fromIntegral (i * 137 + i `quot` 13) | i <- [0 .. n - 1 :: Int]], seed <- seeds]
  , show [hashWithSalt seed text | text <- textSlices, seed <- seeds]
  , show [fst (nextWord64 (mkSMGen seed)) | seed <- [0,1,7,0x1020304050607080,maxBound]]
  ]
  where
    strings :: [Short.ShortText]
    strings = ["", "ASCII", "é", "草原", "A🌳B漢字", "A\NULé🌙", "\x10ffff", "X\xd800\&Y", Short.pack "連結 🌙"]
    seeds = [minBound, -33, 0, 7, 0x102030405060708, maxBound]
    lazyChunks b = Lazy.fromChunks [BS.take 7 b, BS.take 33 (BS.drop 7 b), BS.drop 40 b]
    textSlices = [Text.drop prefix (Text.replicate prefix "Å" <> Short.toText value) | prefix <- [0,1,7,16], value <- strings]
