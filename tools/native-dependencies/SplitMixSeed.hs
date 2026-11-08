{-# LANGUAGE ForeignFunctionInterface #-}

module System.Random.SplitMix.Init (initialSeed) where

import Data.Int (Int32)
import Data.Word (Word32, Word64)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.Storable (peek)

foreign import ccall unsafe "BCryptGenRandom"
  randomBytes :: Ptr () -> Ptr Word64 -> Word32 -> Word32 -> IO Int32

initialSeed :: IO Word64
initialSeed = alloca $ \output -> do
  status <- randomBytes nullPtr output 8 0x00000002 -- BCRYPT_USE_SYSTEM_PREFERRED_RNG
  if status == 0
    then peek output
    else ioError (userError ("BCryptGenRandom could not seed SplitMix, NTSTATUS " ++ show status))
