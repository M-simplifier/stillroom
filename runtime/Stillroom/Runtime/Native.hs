{-# LANGUAGE ForeignFunctionInterface #-}

-- | Stillroom's process identity, display placement and shell dialogs.
module Stillroom.Runtime.Native
  ( ApplicationInstance,
    withApplicationInstance,
    activationRequested,
    activateWindow,
    setPower,
    systemLanguage,
    Monitor (..),
    monitors,
    placeWindow,
    borderlessMatches,
    isUnclipped,
    clientPointer,
    pickFolder,
    pickPicture,
    showProblem,
  )
where

import Control.Exception (bracket, bracketOnError, mask_, throwIO)
import Control.Monad (unless, void, when)
import Data.IORef (modifyIORef', newIORef, readIORef)
import Foreign hiding (void)
import Foreign.C (CWString, CWchar, peekCWString, withCWString)
import GameIO.Platform (Window, nativeWindowHandle, resizeWindow)
import Stillroom.Domain

type Handle = Ptr ()
type WinBool = Int32
type MonitorCallback = Handle -> Handle -> Ptr () -> IntPtr -> IO WinBool
type FolderCallback = Handle -> Word32 -> IntPtr -> IntPtr -> IO Int32

foreign import ccall unsafe "CreateMutexW" createMutex :: Ptr () -> WinBool -> CWString -> IO Handle
foreign import ccall unsafe "CreateEventW" createEvent :: Ptr () -> WinBool -> WinBool -> CWString -> IO Handle
foreign import ccall unsafe "SetEvent" setEvent :: Handle -> IO WinBool
foreign import ccall unsafe "CloseHandle" closeHandle :: Handle -> IO WinBool
foreign import ccall unsafe "WaitForSingleObject" waitObject :: Handle -> Word32 -> IO Word32
foreign import ccall unsafe "GetLastError" lastError :: IO Word32
foreign import ccall unsafe "SetLastError" setLastError :: Word32 -> IO ()
foreign import ccall safe "FindWindowW" findWindow :: CWString -> CWString -> IO Handle
foreign import ccall safe "ShowWindow" showWindow :: Handle -> Int32 -> IO WinBool
foreign import ccall safe "SetForegroundWindow" foregroundWindow :: Handle -> IO WinBool
foreign import ccall unsafe "SetThreadExecutionState" executionState :: Word32 -> IO Word32
foreign import ccall unsafe "GetUserDefaultUILanguage" userLanguage :: IO Word16
foreign import ccall "wrapper" wrapMonitor :: MonitorCallback -> IO (FunPtr MonitorCallback)
foreign import ccall safe "EnumDisplayMonitors"
  enumMonitors :: Handle -> Ptr () -> FunPtr MonitorCallback -> IntPtr -> IO WinBool
foreign import ccall unsafe "GetMonitorInfoW" getMonitorInfo :: Handle -> Ptr () -> IO WinBool
foreign import ccall unsafe "GetWindowLongPtrW" getWindowLong :: Handle -> Int32 -> IO IntPtr
foreign import ccall safe "SetWindowLongPtrW" setWindowLong :: Handle -> Int32 -> IntPtr -> IO IntPtr
foreign import ccall safe "SetWindowPos"
  setWindowPos :: Handle -> Handle -> Int32 -> Int32 -> Int32 -> Int32 -> Word32 -> IO WinBool
foreign import ccall unsafe "CreateRectRgn" createRegion :: Int32 -> Int32 -> Int32 -> Int32 -> IO Handle
foreign import ccall safe "SetWindowRgn" setRegion :: Handle -> Handle -> WinBool -> IO Int32
foreign import ccall unsafe "GetWindowRgn" getRegion :: Handle -> Handle -> IO Int32
foreign import ccall unsafe "GetRgnBox" getRegionBounds :: Handle -> Ptr () -> IO Int32
foreign import ccall unsafe "GetWindowRect" getWindowRect :: Handle -> Ptr () -> IO WinBool
foreign import ccall unsafe "GetClientRect" getClientRect :: Handle -> Ptr () -> IO WinBool
foreign import ccall unsafe "IsWindow" isWindow :: Handle -> IO WinBool
foreign import ccall unsafe "DeleteObject" deleteObject :: Handle -> IO WinBool
foreign import ccall unsafe "GetCursorPos" getCursorPos :: Ptr () -> IO WinBool
foreign import ccall unsafe "ScreenToClient" screenToClient :: Handle -> Ptr () -> IO WinBool
foreign import ccall safe "OleInitialize" oleInitialize :: Ptr () -> IO Int32
foreign import ccall safe "OleUninitialize" oleUninitialize :: IO ()
foreign import ccall unsafe "CoTaskMemFree" taskMemFree :: Ptr () -> IO ()
foreign import ccall "wrapper" wrapFolder :: FolderCallback -> IO (FunPtr FolderCallback)
foreign import ccall safe "SendMessageW" sendMessage :: Handle -> Word32 -> WordPtr -> IntPtr -> IO IntPtr
foreign import ccall safe "SHBrowseForFolderW" browseFolder :: Ptr () -> IO Handle
foreign import ccall safe "SHGetPathFromIDListEx"
  shellPath :: Handle -> CWString -> Word32 -> Word32 -> IO WinBool
foreign import ccall safe "GetOpenFileNameW" openFileDialog :: Ptr () -> IO WinBool
foreign import ccall unsafe "CommDlgExtendedError" dialogError :: IO Word32
foreign import ccall safe "MessageBoxW" messageBox :: Handle -> CWString -> CWString -> Word32 -> IO Int32

newtype ApplicationInstance = ApplicationInstance Handle

-- | On a second launch, wake the running window through an auto-reset event.
-- Call on the application's bound thread so GetLastError observes CreateMutexW.
withApplicationInstance :: (ApplicationInstance -> IO Bool) -> IO Bool
withApplicationInstance use =
  bracket openActivation (void . closeHandle) $ \activation ->
    bracket openMutex (void . closeHandle . fst) $ \(_, status) ->
      if status == 183 -- ERROR_ALREADY_EXISTS
        then do
          require "SetEvent" =<< setEvent activation
          existing <- withCWString "Stillroom" (findWindow nullPtr)
          unless (existing == nullPtr) (activate existing)
          pure True
        else use (ApplicationInstance activation)
  where
    openActivation = withCWString "Local\\Stillroom_Activate_v1" $ \name -> do
      handle <- createEvent nullPtr 0 0 name
      when (handle == nullPtr) (failure "CreateEventW")
      pure handle
    openMutex = withCWString "Local\\Stillroom_Instance_v1" $ \name -> do
      setLastError 0
      handle <- createMutex nullPtr 0 name
      code <- lastError
      when (handle == nullPtr) (failure "CreateMutexW")
      pure (handle, code)

activationRequested :: ApplicationInstance -> IO Bool
activationRequested (ApplicationInstance event) = (== 0) <$> waitObject event 0

activateWindow :: Window -> IO ()
activateWindow window = nativeWindowHandle window >>= activate

activate :: Handle -> IO ()
activate handle = do
  void (showWindow handle 9) -- SW_RESTORE
  void (foregroundWindow handle)

setPower :: Transport -> WindowMode -> IO ()
setPower transportValue mode =
  void (executionState (0x80000000 .|. system .|. display))
  where
    system = if transportValue == Playing then 1 else 0
    display = if transportValue == Playing && mode == Fullscreen then 2 else 0

systemLanguage :: IO Language
systemLanguage = do
  value <- userLanguage
  pure (if value .&. 0x3ff == 0x11 then Japanese else English)

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
  result <- newIORef []
  let visit monitor _ _ _ = allocaBytesAligned 104 4 $ \info -> do
        -- MONITORINFOEXW: cbSize, monitor and work rectangles, flags, WCHAR[32].
        pokeByteOff info 0 (104 :: Word32)
        ok <- getMonitorInfo monitor info
        when (ok /= 0) $ do
          (x, y, r, b) <- peekRect (plusPtr info 4)
          flags <- peekByteOff info 36 :: IO Word32
          name <- peekCWString (plusPtr info 40)
          modifyIORef' result (Monitor name x y (r - x) (b - y) (testBit flags 0) :)
        pure 1
  bracket (wrapMonitor visit) freeHaskellFunPtr $ \callback ->
    require "EnumDisplayMonitors" =<< enumMonitors nullPtr nullPtr callback 0
  reverse <$> readIORef result

placeWindow :: Window -> Monitor -> WindowMode -> IO ()
placeWindow window screen mode = mask_ $ do
  handle <- nativeWindowHandle window
  style <- getWindowLong handle (-16)
  let compactStyle = (style .&. complement 0x80000000) .|. 0x00cf0000
      popupStyle = (style .&. complement 0x00cf0000) .|. 0x80000000
      changeStyle newStyle = when (style /= newStyle) $ do
        setLastError 0
        previous <- setWindowLong handle (-16) newStyle
        when (previous == 0) $ do
          code <- lastError
          unless (code == 0) (throwIO (userError ("SetWindowLongPtrW failed with Windows error " ++ show code)))
      setPosition :: Int -> Int -> Int -> Int -> Word32 -> IO ()
      setPosition left top width height flags =
        require "SetWindowPos" =<< setWindowPos handle nullPtr (fromIntegral left) (fromIntegral top)
          (fromIntegral width) (fromIntegral height) flags
      x = monitorX screen
      y = monitorY screen
      w = monitorWidth screen
      h = monitorHeight screen
      frameFlags = 0x0034 -- SWP_FRAMECHANGED | SWP_NOACTIVATE | SWP_NOZORDER
  case mode of
    Fullscreen -> do
      changeStyle popupStyle
      -- The desktop owns the successfully installed region.
      bracketOnError (region 1 1 (w + 1) (h + 1)) (void . deleteObject) $ \clip ->
        require "SetWindowRgn" =<< setRegion handle clip 0
      setPosition (x - 1) (y - 1) (w + 2) (h + 2) frameFlags
    Compact -> do
      changeStyle compactStyle
      require "SetWindowRgn" =<< setRegion handle nullPtr 1
      -- Enter the selected monitor before measuring its per-monitor client frame.
      setPosition (x + div (w - 760) 2) (y + div (h - 500) 2) 0 0 (frameFlags .|. 1)
      resizeWindow window 760 500

borderlessMatches :: Window -> Monitor -> IO Bool
borderlessMatches window screen = do
  handle <- nativeWindowHandle window
  let x = monitorX screen
      y = monitorY screen
      w = monitorWidth screen
      h = monitorHeight screen
  bracket (region 0 0 0 0) (void . deleteObject) $ \clip ->
    allocaBytesAligned 16 4 $ \outer -> allocaBytesAligned 16 4 $ \visible -> do
      kind <- getRegion handle clip
      ok <- getWindowRect handle outer
      bounds <- getRegionBounds clip visible
      if kind /= 2 || ok == 0 || bounds /= 2
        then pure False
        else (&&) <$> ((== (x - 1, y - 1, x + w + 1, y + h + 1)) <$> peekRect outer)
                  <*> ((== (1, 1, w + 1, h + 1)) <$> peekRect visible)

isUnclipped :: Window -> IO Bool
isUnclipped window = do
  handle <- nativeWindowHandle window
  bracket (region 0 0 0 0) (void . deleteObject) $ \clip ->
    (&&) <$> ((/= 0) <$> isWindow handle) <*> ((== 0) <$> getRegion handle clip)

clientPointer :: Window -> IO (Maybe (Int, Int))
clientPointer window = do
  handle <- nativeWindowHandle window
  allocaBytesAligned 8 4 $ \point -> allocaBytesAligned 16 4 $ \client -> do
    cursor <- getCursorPos point
    mapped <- if cursor == 0 then pure 0 else screenToClient handle point
    bounds <- getClientRect handle client
    if mapped == 0 || bounds == 0
      then pure Nothing
      else do
        x <- fromIntegral <$> (peekByteOff point 0 :: IO Int32)
        y <- fromIntegral <$> (peekByteOff point 4 :: IO Int32)
        (l, t, r, b) <- peekRect client
        pure (if x >= l && x < r && y >= t && y < b then Just (x, y) else Nothing)

region :: Int -> Int -> Int -> Int -> IO Handle
region x y w h = do
  handle <- createRegion (fromIntegral x) (fromIntegral y) (fromIntegral w) (fromIntegral h)
  when (handle == nullPtr) (failure "CreateRectRgn")
  pure handle

peekRect :: Ptr () -> IO (Int, Int, Int, Int)
peekRect ptr = (,,,) <$> field 0 <*> field 4 <*> field 8 <*> field 12
  where field offset = fromIntegral <$> (peekByteOff ptr offset :: IO Int32)

require :: String -> WinBool -> IO ()
require api ok = when (ok == 0) (failure api)

failure :: String -> IO a
failure api = do
  code <- lastError
  throwIO (userError (api ++ " failed with Windows error " ++ show code))

-- | Run on a bound dialog thread. The owner HWND is borrowed from the UI.
pickFolder :: Handle -> Maybe FilePath -> IO (Either String (Maybe FilePath))
pickFolder owner initial =
  bracket (oleInitialize nullPtr) (\status -> when (status >= 0) oleUninitialize) $ \status ->
    if status < 0 then pure failed else
      withCWString (maybe "" id initial) $ \start ->
        withCWString "Choose your music folder" $ \title ->
          bracket (wrapFolder initialized) freeHaskellFunPtr $ \callback ->
            allocaArray 260 $ \(display :: Ptr CWchar) -> allocaBytesAligned 64 8 $ \info -> do
              -- BROWSEINFOW (x64). Initialize every optional pointer.
              fillBytes info 0 64
              pokeByteOff info 0 owner
              pokeByteOff info 16 display
              pokeByteOff info 24 title
              pokeByteOff info 32 (0x251 :: Word32) -- filesystem | new UI | hide New Folder
              pokeByteOff info 40 callback
              pokeByteOff info 48 (ptrToIntPtr start)
              bracket (browseFolder info) taskMemFree $ \chosen ->
                if chosen == nullPtr then pure (Right Nothing) else
                  allocaArray pathCapacity $ \buffer -> do
                    ok <- shellPath chosen buffer (fromIntegral pathCapacity) 0
                    if ok == 0 then pure failed else Right . Just <$> peekCWString buffer
  where
    failed = Left "The folder picker could not open this folder."
    initialized dialog message _ start = do
      when (message == 1 && maybe False (not . null) initial) $
        void (sendMessage dialog 0x467 1 start) -- BFFM_SETSELECTIONW
      pure 0

pickPicture :: Handle -> IO (Either String (Maybe FilePath))
pickPicture owner =
  withCWString "Images (PNG, JPEG, BMP)\0*.png;*.jpg;*.jpeg;*.bmp\0" $ \filters ->
    withCWString "Add a Stillroom background" $ \title ->
      allocaArray pathCapacity $ \buffer -> allocaBytesAligned 152 8 $ \info -> do
        -- OPENFILENAMEW (x64).
        fillBytes info 0 152
        poke buffer (0 :: CWchar)
        pokeByteOff info 0 (152 :: Word32)
        pokeByteOff info 8 owner
        pokeByteOff info 24 filters
        pokeByteOff info 48 buffer
        pokeByteOff info 56 (fromIntegral pathCapacity :: Word32)
        pokeByteOff info 88 title
        pokeByteOff info 96 (0x81808 :: Word32) -- explorer | existing path/file | keep cwd
        ok <- openFileDialog info
        if ok /= 0 then Right . Just <$> peekCWString buffer else do
          status <- dialogError
          pure (if status == 0 then Right Nothing else Left "The image picker could not open.")

pathCapacity :: Int
pathCapacity = 32768

showProblem :: String -> IO ()
showProblem message = withCWString "Stillroom" $ \title ->
  withCWString message $ \body -> void (messageBox nullPtr body title 0x40)
