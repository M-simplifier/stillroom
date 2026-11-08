-- | One lazily acquired GPU world, one pure journey, and the input adapter.
-- All native resources are released on the window thread before its renderer.
module Stillroom.Runtime.Journey
  ( JourneyRuntime,
    withJourney,
    withJourneyForCheck,
    model,
    soundscape,
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
    frameMatchesForCheck,
    frameProjectionErrorForCheck,
    setAllocationLimitsForCheck,
  )
where

import Control.Exception (bracket, mask_, throwIO)
import Control.Monad (unless, when)
import Data.Char (ord)
import Data.IORef
import Data.Maybe (isNothing)
import Data.Vector.Storable qualified as V
import GameIO.Graphics qualified as Native
import GameIO.Input qualified as I
import GameIO.Platform qualified as Platform
import Stillroom.Domain (Language (..))
import Stillroom.Audio.Environment (EnvironmentInput, environmentInput)
import Stillroom.Graphics (Tier)
import Stillroom.Journey.Frame qualified as F
import Stillroom.Graphics.Encoding.Mesh qualified as Mesh
import Stillroom.Journey.Railcar qualified as Railcar
import Stillroom.Journey.Ride qualified as J
import Stillroom.Journey.Walk qualified as W
import Stillroom.Journey.World qualified as World
import Stillroom.Runtime.Graphics.Journey qualified as GPU
import Stillroom.Runtime.Graphics.Journey (FingerprintMode(..))
import Stillroom.Runtime.Graphics.Scene
import Stillroom.Landscape.Geometry qualified as Landscape

-- Geometry is sampled on the frame thread, never in the audio supplier.
-- Sound receives only the few physical quantities it needs to interpolate.
soundscape :: JourneyRuntime -> IO EnvironmentInput
soundscape runtime = do
  j <- model runtime
  let walker = J.view j
      seed = J.worldSeed j
      x = W.posX walker
      z = W.posZ walker
      forest = case W.habitatAt seed x z of
        W.Meadow -> 0.28
        W.Birchwood -> 0.85
        W.Pines -> 0.70
        W.Heath -> 0.20
        W.Wetland -> 0.45
        W.Dunes -> 0.10
  pure (environmentInput (J.seat j == J.Aboard) (J.speed (J.car j)) forest (W.shoreDistance seed x z))

data JourneyRuntime = Runtime
  { window :: !Platform.Window,
    renderer :: !Native.Renderer,
    native :: IORef (Maybe GPU.World),
    current :: IORef J.Journey,
    previous :: IORef J.Journey,
    accumulator :: IORef Double,
    pending :: IORef J.Input,
    remaining :: IORef Int,
    problem :: IORef (Maybe String),
    captured :: IORef Bool,
    hintSeconds :: IORef Double,
    help :: IORef Bool,
    lastPlan :: IORef (Maybe F.FramePlan),
    fingerprintMode :: !FingerprintMode
  }

quality :: JourneyRuntime -> Tier -> IO ()
quality runtime tier = do
  pointer <- ensure runtime
  mapM_ (\world -> GPU.quality world tier) pointer

setAllocationLimitsForCheck :: JourneyRuntime -> Int -> Int -> IO ()
setAllocationLimitsForCheck runtime color depth = do
  pointer <- ensure runtime
  mapM_ (\world -> GPU.setAllocationLimitsForCheck world color depth) pointer

withJourney :: Platform.Window -> Native.Renderer -> (JourneyRuntime -> IO a) -> IO a
withJourney = withJourneyMode SkipFingerprints

withJourneyForCheck :: Platform.Window -> Native.Renderer -> (JourneyRuntime -> IO a) -> IO a
withJourneyForCheck = withJourneyMode CaptureFingerprints

withJourneyMode :: FingerprintMode -> Platform.Window -> Native.Renderer -> (JourneyRuntime -> IO a) -> IO a
withJourneyMode fingerprints owner device = bracket acquire release
  where
    acquire =
      Runtime owner device
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
        <*> newIORef Nothing
        <*> pure fingerprints
    initial = J.initial World.firstSeed
    release runtime = suspend runtime >> releaseWorld runtime

releaseWorld :: JourneyRuntime -> IO ()
releaseWorld r = mask_ $ do
  old <- atomicModifyIORef' (native r) (\p -> (Nothing, p))
  mapM_ GPU.close old

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

fingerprintsForCheck :: JourneyRuntime -> Int -> Int -> IO (Maybe Integer, Maybe Integer)
fingerprintsForCheck r x z = do
  unless (fingerprintMode r == CaptureFingerprints) $
    throwIO (userError "Fingerprint requested from a rendering-only runtime")
  readIORef (native r)
    >>= maybe
      (pure (Nothing, Nothing))
      ( \p ->
          (,)
            <$> (fmap fromIntegral <$> GPU.signature p x z)
            <*> (fmap fromIntegral <$> GPU.farSignature p x z)
      )

terrainMatchesForCheck :: World.Seed -> IO Bool
terrainMatchesForCheck seed = pure (all one [(-16,-16),(-3,2),(0,0),(2,-3),(16,16)])
  where
    one (cx,cz) = case Landscape.seededChunkGeometry seed cx cz Landscape.NearChunk of
      Left _ -> False
      Right geometry ->
        let mesh=Landscape.materialMesh geometry Landscape.Terrain
            positions=Landscape.meshPositions mesh
            cells=Landscape.chunkCells Landscape.NearChunk
            count=cells*cells*6
            sample vertex =
              let x=positions V.! (vertex*3); y=positions V.! (vertex*3+1); z=positions V.! (vertex*3+2)
                  expected=realToFrac (W.terrainHeight seed (realToFrac x) (realToFrac z)) :: Float
               in y==expected
         in Landscape.meshVertexCount mesh>=count && all sample [0,count `div` 4,count `div` 2,count*3 `div` 4,count-1]

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
  Platform.setPointerCapture (window r) False
  when was (Platform.setPointerVisible (window r) True)
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
isCaptured = Platform.windowPointerCaptured . window

-- The host already sampled relative mouse movement; never consume it twice.
update :: JourneyRuntime -> Double -> Bool -> (Float, Float) -> I.InputState -> [I.InputChange] -> IO ()
update r dt click (dx, dy) keys changes = do
  loaded <- (== 0) <$> readIORef (remaining r)
  healthy <- isNothing <$> readIORef (problem r)
  when (loaded && healthy) $ do
    oldCapture <- isCaptured r
    let release = pressed 0x09
    let wanted = not release && (if click then not oldCapture else oldCapture)
    when (wanted /= oldCapture) (Platform.setPointerCapture (window r) wanted)
    capture <- isCaptured r
    was <- atomicModifyIORef' (captured r) (\previously -> (capture, previously))
    when (capture /= was) (Platform.setPointerVisible (window r) (not capture))
    let reveal = pressed 0x70
    when reveal (modifyIORef' (help r) not)
    let w = held (letter 'W')
    let s = held (letter 'S')
    let a = held (letter 'A')
    let d = held (letter 'D')
    let left = held 0x25
    let right = held 0x27
    let space = pressed 0x20
    let access = pressed (letter 'E')
    let up = pressed 0x26
    let down = pressed 0x28
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
  where
    letter = fromIntegral . ord
    held = I.virtualKeyHeld keys
    pressed key = any (\change -> case change of
      I.InputKey I.KeyPressed message -> I.keyVirtual message == key
      _ -> False) changes

ensure :: JourneyRuntime -> IO (Maybe GPU.World)
ensure r = do
  existing <- readIORef (native r)
  failed <- readIORef (problem r)
  case (existing, failed) of
    (Just p, _) -> pure (Just p)
    (_, Just _) -> pure Nothing
    _ -> case railcarPacket of
      Left issue -> writeIORef (problem r) (Just issue) >> pure Nothing
      Right mesh -> mask_ $ do
        j <- model r
        resident <- GPU.openWithFingerprints (renderer r) (fingerprintMode r) (J.worldSeed j) mesh
        writeIORef (native r) (Just resident)
        pure (Just resident)

-- Retain the pure compilation result across world recreation. Shapes are not
-- compiled during drawing; only the GPU resource belongs to a world's lifetime.
railcarPacket :: Either String Mesh.MeshPacket
railcarPacket = do
  mesh <- either (Left . show) Right Railcar.railcarMesh
  either (Left . show) Right (Mesh.lowerMesh mesh)

draw :: JourneyRuntime -> SceneRect -> IO SceneDraw
draw r rectangle@(SceneRect _ _ width height) = do
  let clear = mempty {sceneOverlay = [solid rectangle (Native.Color (13/255) (25/255) (24/255) 1)]}
  pointer <- ensure r
  case pointer of
    Nothing -> pure clear
    Just p -> do
      j <- model r
      before <- readIORef (previous r)
      remainder <- readIORef (accumulator r)
      let shown = J.interpolate (remainder / W.tickSeconds) before j
          v = J.view shown
          c = J.car shown
      missing <- readIORef (remaining r)
      needed <- if missing > 0 then GPU.prepare p (W.posX v) (W.posZ v) else pure missing
      writeIORef (remaining r) needed
      when (needed < 0) (GPU.problem p >>= writeIORef (problem r) . Just)
      if needed /= 0 then pure clear else do
        let rw = max 1 (floor width)
            rh = max 1 (floor height)
        dimensions <- GPU.frameTargets p (W.posX v) (W.posZ v) rw rh
        result <- case dimensions of
          Nothing -> pure (Right mempty)
          Just sizes -> do
            let seed = J.worldSeed shown
                progress = J.progress c
                dx = (J.trackX seed (progress + 0.1) - J.trackX seed (progress - 0.1)) / 0.2
                grade = atan2 (J.trackY seed (progress + 0.1) - J.trackY seed (progress - 0.1)) (0.2 * sqrt (1 + dx*dx))
                input = F.FrameInput
                  (F.Point (W.posX v) (W.eyeY v) (W.posZ v))
                  (W.yaw v) (W.pitch v) (fromIntegral (W.ticks v) * W.tickSeconds)
                  (F.Point (J.trackX seed progress) (J.trackY seed progress) (-progress))
                  (-J.trackHeading seed progress) grade
                planned = either (Left . show) Right (F.planFrame sizes input)
            case planned of
              Left issue -> pure (Left issue)
              Right frame -> do
                writeIORef (lastPlan r) (Just frame)
                Right <$> GPU.render p frame rectangle

        case result of
          Left issue -> writeIORef (problem r) (Just issue) >> pure clear
          Right scene -> do
            ok <- GPU.ready p
            unless ok (GPU.problem p >>= writeIORef (problem r) . Just)
            let fade = if J.fade j > 0 then mempty {sceneOverlay =
                  [solid rectangle (Native.Color (5/255) (10/255) (10/255) (realToFrac (J.fade j)))]} else mempty
            pure (clear <> scene <> fade)

-- Compare the values consumed by the GPU interpreter with the validated plan.
frameMatchesForCheck :: JourneyRuntime -> IO Bool
frameMatchesForCheck r = do
  expected <- readIORef (lastPlan r)
  pointer <- readIORef (native r)
  case pointer of
    Nothing -> pure False
    Just p -> do
      actual <- GPU.executedFrame p
      pure (expected == actual && not (isNothing actual))

frameProjectionErrorForCheck :: JourneyRuntime -> IO (Maybe Float)
frameProjectionErrorForCheck r = do
  pointer <- readIORef (native r)
  frame <- maybe (pure Nothing) GPU.executedFrame pointer
  pure (GPU.clipProjectionError <$> frame)

status :: JourneyRuntime -> Language -> IO (Maybe String)
status r language = do
  failed <- readIORef (problem r)
  missing <- readIORef (remaining r)
  pure $
    if failed /= Nothing
      then Just (local language "Journey unavailable. Esc returns to work." "旅の景色を描画できません。Escで作業に戻ります。")
      else if missing > 0 then Just (local language "Preparing your landscape…" "旅の景色を準備しています…") else Nothing

info :: JourneyRuntime -> IO String
info r = readIORef (native r) >>= maybe (pure "{}") GPU.info

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
