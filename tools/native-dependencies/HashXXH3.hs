{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnliftedFFITypes #-}

-- Windows x64 adaptation of the internal hashable 1.5.1.0 interface.
module Data.Hashable.XXH3
  ( xxh3_64bit_withSeed_ptr, xxh3_64bit_withSeed_bs, xxh3_64bit_withSeed_ba
  , xxh3_64bit_withSeed_w64, xxh3_64bit_withSeed_w32
  , XXH3_State, xxh3_64bit_createState, xxh3_64bit_reset_withSeed
  , xxh3_64bit_digest, xxh3_64bit_update_bs, xxh3_64bit_update_ba
  , xxh3_64bit_update_w64, xxh3_64bit_update_w32
  ) where

import Control.Exception (mask_, onException)
import Control.Monad (unless, void)
import Control.Monad.ST (runST)
import Control.Monad.ST.Unsafe (unsafeIOToST)
import Data.Array.Byte (ByteArray(..))
import qualified Data.ByteString as BS
import Data.ByteString.Unsafe (unsafeUseAsCStringLen)
import Data.Word (Word8, Word32, Word64)
import Foreign.C.Types (CInt(..), CSize(..))
import qualified Foreign.Concurrent as Concurrent
import Foreign.ForeignPtr (ForeignPtr, withForeignPtr)
import Foreign.Marshal.Utils (with)
import Foreign.Ptr (Ptr, castPtr, nullPtr)
import Foreign.Storable (Storable, sizeOf)
import GHC.Exts (ByteArray#, Int(..), copyByteArray#, newByteArray#, unsafeFreezeByteArray#)
import GHC.ST (ST(..))
import System.IO.Unsafe (unsafeDupablePerformIO)

foreign import ccall unsafe "__rts_XXH3_64bits_withSeed"
  hashPointer :: Ptr Word8 -> CSize -> Word64 -> IO Word64
foreign import ccall unsafe "__rts_XXH3_64bits_withSeed"
  hashArray :: ByteArray# -> CSize -> Word64 -> Word64
foreign import ccall unsafe "__rts_XXH3_createState"
  createState :: IO (Ptr ())
foreign import ccall unsafe "__rts_XXH3_freeState"
  freeState :: Ptr () -> IO CInt
foreign import ccall unsafe "__rts_XXH3_64bits_reset_withSeed"
  resetState :: Ptr () -> Word64 -> IO CInt
foreign import ccall unsafe "__rts_XXH3_64bits_digest"
  digestState :: Ptr () -> IO Word64
foreign import ccall unsafe "__rts_XXH3_64bits_update"
  updatePointer :: Ptr () -> Ptr Word8 -> CSize -> IO CInt
foreign import ccall unsafe "__rts_XXH3_64bits_update"
  updateArray :: Ptr () -> ByteArray# -> CSize -> IO CInt

xxh3_64bit_withSeed_ptr :: Ptr Word8 -> Int -> Word64 -> IO Word64
xxh3_64bit_withSeed_ptr ptr count = hashPointer ptr (fromIntegral count)

xxh3_64bit_withSeed_bs :: BS.ByteString -> Word64 -> Word64
xxh3_64bit_withSeed_bs bytes seed = unsafeDupablePerformIO $
  unsafeUseAsCStringLen bytes $ \(ptr, count) -> xxh3_64bit_withSeed_ptr (castPtr ptr) count seed

xxh3_64bit_withSeed_ba :: ByteArray -> Int -> Int -> Word64 -> Word64
xxh3_64bit_withSeed_ba bytes offset count seed =
  case slice bytes offset count of
    ByteArray bytes# -> hashArray bytes# (fromIntegral count) seed

xxh3_64bit_withSeed_w64 :: Word64 -> Word64 -> Word64
xxh3_64bit_withSeed_w64 = hashWord

xxh3_64bit_withSeed_w32 :: Word32 -> Word64 -> Word64
xxh3_64bit_withSeed_w32 = hashWord

hashWord :: Storable a => a -> Word64 -> Word64
hashWord word seed = unsafeDupablePerformIO $
  with word $ \ptr -> xxh3_64bit_withSeed_ptr (castPtr ptr) (sizeOf word) seed

type role XXH3_State nominal
newtype XXH3_State s = XXH3 (ForeignPtr ())

xxh3_64bit_createState :: ST s (XXH3_State s)
xxh3_64bit_createState = unsafeIOToST $ mask_ $ do
  ptr <- createState
  unless (ptr /= nullPtr) (ioError (userError "GHC RTS XXH3 state allocation failed"))
  let cleanup = void (freeState ptr)
  XXH3 <$> (Concurrent.newForeignPtr ptr cleanup `onException` cleanup)

xxh3_64bit_reset_withSeed :: XXH3_State s -> Word64 -> ST s ()
xxh3_64bit_reset_withSeed (XXH3 state) seed = unsafeIOToST $
  withForeignPtr state $ \ptr -> checked (resetState ptr seed)

xxh3_64bit_digest :: XXH3_State s -> ST s Word64
xxh3_64bit_digest (XXH3 state) = unsafeIOToST (withForeignPtr state digestState)

xxh3_64bit_update_bs :: XXH3_State s -> BS.ByteString -> ST s ()
xxh3_64bit_update_bs (XXH3 state) bytes = unless (BS.null bytes) $ unsafeIOToST $
  withForeignPtr state $ \ptr -> unsafeUseAsCStringLen bytes $ \(source, count) ->
    checked (updatePointer ptr (castPtr source) (fromIntegral count))

xxh3_64bit_update_ba :: XXH3_State s -> ByteArray -> Int -> Int -> ST s ()
xxh3_64bit_update_ba (XXH3 state) bytes offset count = unless (count == 0) $ unsafeIOToST $
  withForeignPtr state $ \ptr -> case slice bytes offset count of
    ByteArray bytes# -> checked (updateArray ptr bytes# (fromIntegral count))

xxh3_64bit_update_w64 :: XXH3_State s -> Word64 -> ST s ()
xxh3_64bit_update_w64 = updateWord

xxh3_64bit_update_w32 :: XXH3_State s -> Word32 -> ST s ()
xxh3_64bit_update_w32 = updateWord

updateWord :: Storable a => XXH3_State s -> a -> ST s ()
updateWord (XXH3 state) word = unsafeIOToST $
  withForeignPtr state $ \ptr -> with word $ \source ->
    checked (updatePointer ptr (castPtr source) (fromIntegral (sizeOf word)))

checked :: IO CInt -> IO ()
checked action = do
  status <- action
  unless (status == 0) (ioError (userError ("GHC RTS XXH3 status " ++ show status)))

-- Unlifted unsafe FFI borrows the whole byte array directly. Use an exact
-- slice for nonzero offsets without carrying a movable interior pointer.
slice :: ByteArray -> Int -> Int -> ByteArray
slice bytes 0 _ = bytes
slice (ByteArray source#) (I# offset#) (I# count#) = runST $ ST $ \s0 ->
  case newByteArray# count# s0 of
    (# s1, target# #) -> case copyByteArray# source# offset# target# 0# count# s1 of
      s2 -> case unsafeFreezeByteArray# target# s2 of
        (# s3, result# #) -> (# s3, ByteArray result# #)
