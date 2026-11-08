-- Keep bulk fills for bytes and numeric zero, including the upstream Float
-- and Double convention of storing positive zero for a signed-zero fill.
fillOctetArray# :: Integral a => MutableByteArray# s -> Int# -> Int# -> a -> State# s -> State# s
{-# INLINE fillOctetArray# #-}
fillOctetArray# array# offset# count# value state
  | isTrue# (count# <=# 0#) = state
  | otherwise = case (fromIntegral value :: Int) of
      I# byte# -> GHC.Exts.setByteArray# array# offset# count# byte# state

fillNumericArray# :: forall a s. (Prim a, Eq a, Num a) => MutableByteArray# s -> Int# -> Int# -> a -> State# s -> State# s
{-# INLINE fillNumericArray# #-}
fillNumericArray# array# offset# count# value state
  | isTrue# (count# <=# 0#) = state
  | value == 0 = case sizeOfType# (Proxy :: Proxy a) of
      size# -> GHC.Exts.setByteArray# array# (offset# *# size#) (count# *# size#) 0# state
  | otherwise = defaultSetByteArray# array# offset# count# value state

fillOctetAddress# :: Integral a => Addr# -> Int# -> Int# -> a -> State# s -> State# s
{-# INLINE fillOctetAddress# #-}
fillOctetAddress# address# offset# count# value =
  fillAddress# address# offset# count# (fromIntegral value)

fillNumericAddress# :: forall a s. (Prim a, Eq a, Num a) => Addr# -> Int# -> Int# -> a -> State# s -> State# s
{-# INLINE fillNumericAddress# #-}
fillNumericAddress# address# offset# count# value state
  | value == 0 = case sizeOfType# (Proxy :: Proxy a) of
      size# -> fillAddress# address# (offset# *# size#) (count# *# size#) 0 state
  | otherwise = defaultSetOffAddr# address# offset# count# value state

fillAddress# :: Addr# -> Int# -> Int# -> Word8 -> State# s -> State# s
{-# INLINE fillAddress# #-}
fillAddress# address# offset# count# value state
  | isTrue# (count# <=# 0#) = state
  | otherwise =
      case unsafeCoerce# (internal (Memory.fillBytes (Ptr (plusAddr# address# offset#)) value (I# count#))) state of
        (# next, _ #) -> next
