{-# LANGUAGE ForeignFunctionInterface #-}

module Stillroom.Runtime.Native
  ( beginInstance,
    endInstance,
    activationRequested,
    setPower,
    systemLanguage,
    Monitor (..),
    monitors,
    positionWindow,
    unclipWindow,
    borderlessMatches,
    isUnclipped,
    pickFolder,
    pickPicture,
    showProblem,
  )
where

import Control.Exception (throwIO)
import Control.Monad (forM, when)
import Data.Maybe (catMaybes)
import Foreign
import Foreign.C
import Stillroom.Domain

foreign import ccall unsafe "sr_begin" beginRaw :: IO CInt

foreign import ccall unsafe "sr_end" endInstance :: IO ()

foreign import ccall unsafe "sr_activated" activatedRaw :: IO CInt

foreign import ccall unsafe "sr_power" powerRaw :: CInt -> CInt -> IO ()

foreign import ccall unsafe "sr_japanese" japaneseRaw :: IO CInt

foreign import ccall unsafe "sr_monitors" monitorCountRaw :: IO CInt

foreign import ccall unsafe "sr_monitor" monitorRaw :: CInt -> Ptr CInt -> CWString -> CInt -> IO CInt

foreign import ccall unsafe "sr_position" positionRaw :: Ptr () -> CInt -> CInt -> CInt -> CInt -> IO CInt

foreign import ccall unsafe "sr_unclip" unclipRaw :: Ptr () -> IO CInt

foreign import ccall unsafe "sr_borderless_matches" borderlessRaw :: Ptr () -> CInt -> CInt -> CInt -> CInt -> IO CInt

foreign import ccall unsafe "sr_is_unclipped" unclippedRaw :: Ptr () -> IO CInt

foreign import ccall safe "sr_pick_folder" pickRaw :: Ptr () -> CWString -> CWString -> CInt -> IO CInt

foreign import ccall safe "sr_pick_picture" pickPictureRaw :: Ptr () -> CWString -> CInt -> IO CInt

foreign import ccall safe "sr_message" messageRaw :: CWString -> IO ()

-- Left: OS failure; Right False: existing instance was activated.
beginInstance :: IO (Either String Bool)
beginInstance = do
  value <- beginRaw
  pure $ if value < 0 then Left "Cannot create the application instance lock." else Right (value == 1)

activationRequested :: IO Bool
activationRequested = (/= 0) <$> activatedRaw

setPower :: Transport -> WindowMode -> IO ()
setPower p w = powerRaw (if p == Playing then 1 else 0) (if w == Fullscreen then 1 else 0)

systemLanguage :: IO Language
systemLanguage = do j <- japaneseRaw; pure (if j /= 0 then Japanese else English)

data Monitor = Monitor
  { monitorName :: !String,
    monitorX :: !Int,
    monitorY :: !Int,
    monitorWidth :: !Int,
    monitorHeight :: !Int,
    monitorPrimary :: !Bool
  }
  deriving (Eq, Show)

monitors :: IO [Monitor]
monitors = do
  count <- monitorCountRaw
  catMaybes
    <$> forM
      [0 .. fromIntegral count - 1]
      ( \index -> allocaArray 5 $ \values -> allocaArray 32 $ \name -> do
          ok <- monitorRaw index values name 32
          if ok == 0
            then pure Nothing
            else do
              fields <- map fromIntegral <$> peekArray 5 values
              title <- peekCWString name
              pure $ case fields of
                [x, y, w, h, p] -> Just (Monitor title x y w h (p /= 0))
                _ -> Nothing
      )

positionWindow :: Ptr () -> Int -> Int -> Int -> Int -> IO ()
positionWindow handle x y w h = do
  ok <- positionRaw handle (fromIntegral x) (fromIntegral y) (fromIntegral w) (fromIntegral h)
  when (ok == 0) (throwIO (userError "Could not place the borderless window."))

unclipWindow :: Ptr () -> IO ()
unclipWindow handle = do
  ok <- unclipRaw handle
  when (ok == 0) (throwIO (userError "Could not restore the compact window."))

borderlessMatches :: Ptr () -> Monitor -> IO Bool
borderlessMatches handle m = (/= 0) <$> borderlessRaw handle (fromIntegral (monitorX m)) (fromIntegral (monitorY m)) (fromIntegral (monitorWidth m)) (fromIntegral (monitorHeight m))

isUnclipped :: Ptr () -> IO Bool
isUnclipped handle = (/= 0) <$> unclippedRaw handle

pickFolder :: Ptr () -> Maybe FilePath -> IO (Either String (Maybe FilePath))
pickFolder owner initial = withCWString (maybe "" id initial) $ \start -> allocaArray 32768 $ \buffer -> do
  status <- pickRaw owner start buffer 32768
  case status of
    0 -> pure (Right Nothing)
    1 -> Right . Just <$> peekCWString buffer
    _ -> pure (Left "The folder picker could not open this folder.")

showProblem :: String -> IO ()
showProblem message = withCWString message messageRaw

pickPicture :: Ptr () -> IO (Either String (Maybe FilePath))
pickPicture owner = allocaArray 32768 $ \buffer -> do
  status <- pickPictureRaw owner buffer 32768
  case status of
    0 -> pure (Right Nothing)
    1 -> Right . Just <$> peekCWString buffer
    _ -> pure (Left "The image picker could not open.")
