{-# LANGUAGE ForeignFunctionInterface #-}

module Stillroom.Runtime.Graphics.GL
  ( finish,
    getError,
    clearErrors,
    integers,
    rendererName,
    depthDescription,
    readUniform,
    QueryFunctions (..),
    queryFunctions,
  )
where

import Control.Monad (when)
import Foreign
import Foreign.C
import Raylib.Util.RLGL qualified as RL

foreign import ccall safe "glFinish" finish :: IO ()

foreign import ccall unsafe "glGetError" getError :: IO CUInt

foreign import ccall unsafe "glGetIntegerv" getIntegers :: CUInt -> Ptr CInt -> IO ()

foreign import ccall unsafe "glGetString" getString :: CUInt -> IO CString

foreign import ccall unsafe "glBindTexture" bindTexture :: CUInt -> CUInt -> IO ()

foreign import ccall unsafe "glGetTexLevelParameteriv" textureParameter :: CUInt -> CInt -> CUInt -> Ptr CInt -> IO ()

foreign import ccall unsafe "dynamic" uniformCall :: FunPtr (CUInt -> CInt -> Ptr CFloat -> IO ()) -> CUInt -> CInt -> Ptr CFloat -> IO ()

clearErrors :: IO ()
clearErrors = getError >>= \e -> when (e /= 0) clearErrors

integers :: CUInt -> Int -> IO [Int]
integers key count = allocaArray count $ \p -> getIntegers key p >> map fromIntegral <$> peekArray count p

rendererName :: IO String
rendererName = getString 0x1f01 >>= \p -> if p == nullPtr then pure "unknown" else peekCString p

depthDescription :: Integer -> IO [Int]
depthDescription texture = do
  previous <- integers 0x8069 1
  bindTexture 0x0de1 (fromIntegral texture)
  values <- mapM (\key -> alloca $ \p -> textureParameter 0x0de1 0 key p >> fromIntegral <$> peek p) [0x1003, 0x884a, 0x8c16]
  case previous of
    [p] -> bindTexture 0x0de1 (fromIntegral p)
    _ -> pure ()
  pure values

readUniform :: Integer -> Int -> Int -> IO (Maybe [Float])
readUniform program location count = do
  proc <- RL.rlGetProcAddress "glGetUniformfv"
  if proc == nullPtr || location < 0
    then pure Nothing
    else allocaArray count $ \output -> do
      uniformCall (castPtrToFunPtr proc) (fromIntegral program) (fromIntegral location) output
      values <- map realToFrac <$> peekArray count output
      e <- getError
      pure (if e == 0 then Just values else Nothing)

type Gen = CInt -> Ptr CUInt -> IO ()

type Delete = CInt -> Ptr CUInt -> IO ()

type Begin = CUInt -> CUInt -> IO ()

type End = CUInt -> IO ()

type Available = CUInt -> CUInt -> Ptr CInt -> IO ()

type Result = CUInt -> CUInt -> Ptr Word64 -> IO ()

foreign import ccall unsafe "dynamic" genCall :: FunPtr Gen -> Gen

foreign import ccall unsafe "dynamic" deleteCall :: FunPtr Delete -> Delete

foreign import ccall unsafe "dynamic" beginCall :: FunPtr Begin -> Begin

foreign import ccall unsafe "dynamic" endCall :: FunPtr End -> End

foreign import ccall unsafe "dynamic" availableCall :: FunPtr Available -> Available

foreign import ccall unsafe "dynamic" resultCall :: FunPtr Result -> Result

data QueryFunctions = QueryFunctions
  { generateQueries :: Gen,
    deleteQueries :: Delete,
    beginQuery :: Begin,
    endQuery :: End,
    queryAvailable :: Available,
    queryResult :: Result
  }

queryFunctions :: IO (Maybe QueryFunctions)
queryFunctions = do
  pointers <- mapM RL.rlGetProcAddress ["glGenQueries", "glDeleteQueries", "glBeginQuery", "glEndQuery", "glGetQueryObjectiv", "glGetQueryObjectui64v"]
  pure $ case pointers of
    [a, b, c, d, e, f]
      | all (/= nullPtr) pointers ->
          Just
            ( QueryFunctions
                (genCall (castPtrToFunPtr a))
                (deleteCall (castPtrToFunPtr b))
                (beginCall (castPtrToFunPtr c))
                (endCall (castPtrToFunPtr d))
                (availableCall (castPtrToFunPtr e))
                (resultCall (castPtrToFunPtr f))
            )
    _ -> Nothing
