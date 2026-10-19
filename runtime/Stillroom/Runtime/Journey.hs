{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE PatternSynonyms #-}

-- | One lazily acquired GPU world, one pure journey, and the input adapter.
-- All native resources are released on the window thread before its GL context.
module Stillroom.Runtime.Journey
  ( JourneyRuntime,
    withJourney,
    model,
    replace,
    enter,
    suspend,
    update,
    draw,
    status,
    info,
    isCaptured,
    hints,
    interfaceText,
    restoreModel,
    unload,
    advanceForCheck,
    fingerprintsForCheck,
    terrainMatchesForCheck,
    quality,
  )
where

import Control.Exception (IOException, bracket, try)
import Control.Monad (unless, when)
import Data.ByteString qualified as BS
import Data.IORef
import Data.Maybe (isNothing)
import Foreign (Ptr, nullPtr, withArray, withMany)
import Foreign.C (CDouble (..), CFloat (..), CInt (..), CString, CUInt (..), CULLong (..), peekCString)
import Raylib.Core qualified as R
import Raylib.Core.Shapes (drawRectangleRec)
import Raylib.Types
import Stillroom.Domain (Language (..))
import Stillroom.Graphics (Tier)
import Stillroom.Journey.Ride qualified as J
import Stillroom.Journey.Walk qualified as W
import Stillroom.Journey.World qualified as World
import Stillroom.Runtime.Pictures qualified as Pictures
import System.FilePath ((</>))

data NativeWorld

data JourneyRuntime = Runtime
  { native :: IORef (Maybe (Ptr NativeWorld)),
    current :: IORef J.Journey,
    previous :: IORef J.Journey,
    accumulator :: IORef Double,
    pending :: IORef J.Input,
    remaining :: IORef Int,
    problem :: IORef (Maybe String),
    captured :: IORef Bool,
    hintSeconds :: IORef Double,
    help :: IORef Bool
  }

foreign import ccall safe "sr_journey_open" openWorld :: CUInt -> Ptr CString -> IO (Ptr NativeWorld)

foreign import ccall safe "sr_journey_close" closeWorld :: Ptr NativeWorld -> IO ()

foreign import ccall unsafe "sr_journey_ready" ready :: Ptr NativeWorld -> IO CInt

foreign import ccall unsafe "sr_journey_error" nativeError :: Ptr NativeWorld -> IO CString

foreign import ccall safe "sr_journey_prepare" prepare :: Ptr NativeWorld -> CDouble -> CDouble -> IO CInt

foreign import ccall safe "sr_journey_draw" render :: Ptr NativeWorld -> CDouble -> CDouble -> CDouble -> CDouble -> CDouble -> CDouble -> CFloat -> CFloat -> CInt -> CInt -> IO CInt

foreign import ccall unsafe "sr_car_car" setCar :: Ptr NativeWorld -> CDouble -> CDouble -> CInt -> IO ()

foreign import ccall unsafe "sr_journey_info" nativeInfo :: Ptr NativeWorld -> IO CString

foreign import ccall unsafe "sr_journey_signature" nativeSignature :: Ptr NativeWorld -> CInt -> CInt -> IO CULLong

foreign import ccall unsafe "sr_journey_far_signature" nativeFarSignature :: Ptr NativeWorld -> CInt -> CInt -> IO CULLong

foreign import ccall unsafe "sr_journey_geometry_height" geometryHeight :: CUInt -> CDouble -> CDouble -> IO CDouble

foreign import ccall safe "sr_journey_quality" nativeQuality :: Ptr NativeWorld -> CInt -> IO CInt

quality :: JourneyRuntime -> Tier -> IO ()
quality runtime tier = do
  pointer <- ensure runtime
  case pointer of
    Nothing -> pure ()
    Just p -> do
      ok <- nativeQuality p (fromIntegral (fromEnum tier))
      unless (ok == 1) (nativeError p >>= peekCString >>= writeIORef (problem runtime) . Just)

withJourney :: (JourneyRuntime -> IO a) -> IO a
withJourney = bracket acquire release
  where
    acquire =
      Runtime
        <$> newIORef Nothing
        <*> newIORef initial
        <*> newIORef initial
        <*> newIORef 0
        <*> newIORef J.neutral
        <*> newIORef 130
        <*> newIORef Nothing
        <*> newIORef False
        <*> newIORef 12
        <*> newIORef False
    initial = J.initial World.firstSeed
    release runtime = suspend runtime >> releaseWorld runtime

releaseWorld :: JourneyRuntime -> IO ()
releaseWorld r = do
  old <- atomicModifyIORef' (native r) (\p -> (Nothing, p))
  mapM_ closeWorld old

-- Switching to a quiet scene releases the journey's substantial mesh cache.
-- Returning to work keeps it warm for the next short break.
unload :: JourneyRuntime -> IO ()
unload r = do
  suspend r
  releaseWorld r
  writeIORef (remaining r) 130
  writeIORef (problem r) Nothing

-- Diagnostic adapters use the production reducer and renderer. They do not
-- read user files or alter semantic time in the ordinary input loop.
advanceForCheck :: JourneyRuntime -> Int -> J.Input -> IO ()
advanceForCheck r count input = do
  j <- model r
  let (_, next) = J.simulate count input j j
  writeIORef (current r) next
  writeIORef (previous r) next
  writeIORef (accumulator r) 0

fingerprintsForCheck :: JourneyRuntime -> Int -> Int -> IO (Integer, Integer)
fingerprintsForCheck r x z =
  readIORef (native r)
    >>= maybe
      (pure (0, 0))
      ( \p ->
          (,)
            <$> (fromIntegral <$> nativeSignature p (fromIntegral x) (fromIntegral z))
            <*> (fromIntegral <$> nativeFarSignature p (fromIntegral x) (fromIntegral z))
      )

terrainMatchesForCheck :: World.Seed -> IO Bool
terrainMatchesForCheck seed = and <$> mapM one [(x, z) | x <- [-512, -81, 0, 81, 512], z <- [-512, 80, 145, 260, 512]]
  where
    one (x, z) = do
      height <- geometryHeight (fromIntegral (World.seedValue seed)) (realToFrac x) (realToFrac z)
      pure (abs (realToFrac height - W.terrainHeight seed x z) < 1e-8)

model :: JourneyRuntime -> IO J.Journey
model = readIORef . current

replace :: JourneyRuntime -> World.Seed -> IO ()
replace r = restoreModel r . J.initial

restoreModel :: JourneyRuntime -> J.Journey -> IO ()
restoreModel r j = do
  suspend r
  releaseWorld r
  writeIORef (current r) j
  writeIORef (previous r) j
  writeIORef (remaining r) 130
  writeIORef (problem r) Nothing

enter :: JourneyRuntime -> IO ()
enter r = do
  writeIORef (hintSeconds r) 12
  writeIORef (accumulator r) 0
  writeIORef (pending r) J.neutral

-- Pause elapsed time and cursor capture while preserving the choice to travel.
-- Re-entry accelerates gently from the saved spot, without catching up wall time.
suspend :: JourneyRuntime -> IO ()
suspend r = do
  was <- atomicModifyIORef' (captured r) (\x -> (False, x))
  when was R.enableCursor
  j <- model r
  let c = J.car j
      w = J.person j
      frozen =
        j
          { J.car = c {J.speed = 0, J.acceleration = 0},
            J.exitRequested = False,
            J.transferTicks = 0,
            J.transferTarget = Nothing,
            J.person = w {W.velocityX = 0, W.velocityZ = 0}
          }
  writeIORef (current r) frozen
  writeIORef (previous r) frozen
  writeIORef (accumulator r) 0
  writeIORef (pending r) J.neutral
  writeIORef (help r) False

isCaptured :: JourneyRuntime -> IO Bool
isCaptured = readIORef . captured

-- The host already sampled relative mouse movement; never consume it twice.
update :: JourneyRuntime -> Double -> Bool -> (Float, Float) -> IO ()
update r dt click (dx, dy) = do
  loaded <- (== 0) <$> readIORef (remaining r)
  healthy <- isNothing <$> readIORef (problem r)
  when (loaded && healthy) $ do
    oldCapture <- isCaptured r
    release <- R.isKeyPressed KeyTab
    let capture = not release && (if click then not oldCapture else oldCapture)
    when (capture /= oldCapture) $ do
      (if capture then R.disableCursor else R.enableCursor)
      writeIORef (captured r) capture
    reveal <- R.isKeyPressed KeyF1
    when reveal (modifyIORef' (help r) not)
    w <- R.isKeyDown KeyW
    s <- R.isKeyDown KeyS
    a <- R.isKeyDown KeyA
    d <- R.isKeyDown KeyD
    left <- R.isKeyDown KeyLeft
    right <- R.isKeyDown KeyRight
    space <- R.isKeyPressed KeySpace
    access <- R.isKeyPressed KeyE
    up <- R.isKeyPressed KeyUp
    down <- R.isKeyPressed KeyDown
    waiting <- readIORef (pending r)
    oldAccumulator <- readIORef (accumulator r)
    let elapsed = if dt > 0.5 then 0 else max 0 dt
        wi = J.walking waiting
        value b = if b then 1 else 0
        input =
          J.Input
            ( W.Input
                (value w - value s)
                (value d - value a)
                (W.lookX wi + (if capture && oldCapture then realToFrac dx else 0) + (value right - value left) * 357 * elapsed)
                (W.lookY wi + if capture && oldCapture then realToFrac dy else 0)
                (W.toggleStroll wi || space)
            )
            (J.access waiting || access)
            (if up || down then round (value up - value down :: Double) else J.changePace waiting)
        total = oldAccumulator + elapsed
        count = min 6 (floor (total / W.tickSeconds))
        rest = max 0 (min W.tickSeconds (total - fromIntegral count * W.tickSeconds))
        changed = click || release || reveal || space || access || up || down
    modifyIORef' (hintSeconds r) (\old -> if changed then 6 else max 0 (old - elapsed))
    p <- readIORef (previous r)
    c <- model r
    let (before, after) = J.simulate count input p c
    writeIORef (previous r) before
    writeIORef (current r) after
    writeIORef (accumulator r) rest
    writeIORef (pending r) (if count == 0 then input else J.neutral)

ensure :: JourneyRuntime -> IO (Maybe (Ptr NativeWorld))
ensure r = do
  existing <- readIORef (native r)
  failed <- readIORef (problem r)
  case (existing, failed) of
    (Just p, _) -> pure (Just p)
    (_, Just _) -> pure Nothing
    _ -> do
      sources <-
        try $
          mapM
            (\name -> Pictures.assetPath ("assets" </> "rest" </> "journey" </> name) >>= BS.readFile)
            ["common.glsl", "land.vs", "land.fs", "water.vs", "water.fs", "sky.fs", "post.fs"]
      case sources of
        Left (_ :: IOException) -> writeIORef (problem r) (Just "Journey shaders unavailable") >> pure Nothing
        Right files -> do
          j <- model r
          pointer <- withMany BS.useAsCString files (\strings -> withArray strings (openWorld (fromIntegral (World.seedValue (J.worldSeed j)))))
          if pointer == nullPtr
            then writeIORef (problem r) (Just "Journey allocation failed") >> pure Nothing
            else do
              writeIORef (native r) (Just pointer)
              ok <- ready pointer
              unless (ok == 1) (nativeError pointer >>= peekCString >>= writeIORef (problem r) . Just)
              pure (Just pointer)

draw :: JourneyRuntime -> Rectangle -> IO ()
draw r rectangle@(Rectangle left top width height) = do
  drawRectangleRec rectangle (Color 13 25 24 255)
  pointer <- ensure r
  case pointer of
    Nothing -> pure ()
    Just p -> do
      j <- model r
      before <- readIORef (previous r)
      remainder <- readIORef (accumulator r)
      let shown = J.interpolate (remainder / W.tickSeconds) before j
          v = J.view shown
          c = J.car shown
      missing <- readIORef (remaining r)
      needed <- if missing > 0 then fromIntegral <$> prepare p (realToFrac (W.posX v)) (realToFrac (W.posZ v)) else pure missing
      writeIORef (remaining r) needed
      when (needed < 0) (nativeError p >>= peekCString >>= writeIORef (problem r) . Just)
      when (needed == 0) $ do
        setCar p (realToFrac (J.progress c)) (realToFrac (J.speed c)) (if J.seat shown == J.Aboard then 1 else 0)
        ok <-
          render
            p
            (realToFrac (W.posX v))
            (realToFrac (W.eyeY v))
            (realToFrac (W.posZ v))
            (realToFrac (W.yaw v))
            (realToFrac (W.pitch v))
            (realToFrac (fromIntegral (W.ticks v) * W.tickSeconds))
            (realToFrac left)
            (realToFrac top)
            (max 1 (floor width))
            (max 1 (floor height))
        unless (ok == 1) (nativeError p >>= peekCString >>= writeIORef (problem r) . Just)
        when (J.fade j > 0) (drawRectangleRec rectangle (Color 5 10 10 (round (255 * J.fade j))))

status :: JourneyRuntime -> Language -> IO (Maybe String)
status r language = do
  failed <- readIORef (problem r)
  missing <- readIORef (remaining r)
  pure $
    if failed /= Nothing
      then Just (local language "Journey unavailable. Esc returns to work." "旅の景色を描画できません。Escで作業に戻ります。")
      else if missing > 0 then Just (local language "Preparing your landscape…" "旅の景色を準備しています…") else Nothing

info :: JourneyRuntime -> IO String
info r = readIORef (native r) >>= maybe (pure "{}") (\p -> nativeInfo p >>= peekCString)

local :: Language -> String -> String -> String
local Japanese _ ja = ja
local English en _ = en

-- Text is shared with the font atlas and the actual overlay.
hints :: JourneyRuntime -> Language -> IO [String]
hints r language = do
  j <- model r
  seconds <- readIORef (hintSeconds r)
  allHelp <- readIORef (help r)
  let loc = local language
      v = J.person j
      atLimit = J.progress (J.car j) >= World.coordinateLimit - 0.1 || max (abs (W.posX v)) (abs (W.posZ v)) >= World.coordinateLimit - 1
      blocked = J.seat j == J.Aboard && not (J.running (J.car j)) && J.speed (J.car j) == 0 && J.noticeTicks j > 0 && isNothing (J.landing (J.worldSeed j) (J.car j))
      explanation
        | atLimit = loc "Landscape boundary. Return to settings for a new landscape." "この景色の端です。設定から新しい景色を始められます。"
        | J.exitRequested j = loc "Slowing down to step outside…" "停車してから、外へ降ります…"
        | blocked = loc "Stay aboard here. Space continues to safer ground." "ここでは降りられません。Spaceで次の岸辺へ。"
        | J.seat j == J.Aboard = loc "Space  pause / ride     E  step outside     ↑ ↓  pace" "Space  停車・出発     E  降りる     ↑ ↓  速さ"
        | J.nearCar j = loc "E  board     WASD  walk     Space  automatic walk" "E  乗る     WASD  歩く     Space  自動散歩"
        | otherwise = loc "WASD  walk     Space  automatic walk" "WASD  歩く     Space  自動散歩"
  pure $
    if allHelp || seconds > 0 || J.exitRequested j || J.noticeTicks j > 0 || atLimit
      then [explanation, loc "Click  look around / release     ← →  look     F1  help     Esc  return" "クリック  視点操作・解除     ← →  見回す     F1  操作説明     Esc  戻る"]
      else [loc "Esc  return to work     F1  help" "Esc  作業に戻る     F1  操作説明"]

interfaceText :: String
interfaceText = "旅の景色を描画できません。Escで作業に戻ります。旅の景色を準備しています…停車してから、外へ降ります…ここでは降りられません。Spaceで次の岸辺へ。Space停車・出発E降りる↑↓速さE乗るWASD歩くSpace自動散歩クリック視点操作・解除←→見回すF1操作説明Esc戻る作業に戻る新しい景色旅を続けるこの景色の端です。設定から新しい景色を始められます。"
