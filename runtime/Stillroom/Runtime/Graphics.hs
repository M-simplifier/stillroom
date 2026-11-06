-- | The effect boundary for quality. Only completed queries affect the pure
-- governor; settings previews and frozen/hidden frames cannot bias it upward.
module Stillroom.Runtime.Graphics (Graphics, withGraphics, choose, measured, diagnostics) where

import Control.Exception (bracket, bracket_, mask_)
import Control.Monad (when)
import Data.IORef
import Foreign
import Foreign.C (CUInt, CInt)
import Raylib.Util.RLGL qualified as RL
import Stillroom.Runtime.Graphics.GL qualified as GL
import Stillroom.Domain (RestScene)
import Stillroom.Graphics

data Timer = Timer !GL.QueryFunctions ![CUInt] !(IORef ([Int], Maybe Int, Int))
data Graphics = Graphics !(Maybe Timer) !(IORef (Maybe (GraphicsQuality, RestScene, Int, Int), Governor)) !(IORef (Int, Double))

open :: IO (Maybe Timer)
open = mask_ $ do
  functions <- GL.queryFunctions
  case functions of
    Nothing -> pure Nothing
    Just f -> allocaArray 4 $ \p -> do
      GL.generateQueries f 4 p
      ids <- peekArray 4 p
      Just . Timer f ids <$> newIORef ([], Nothing, 0)

close :: Maybe Timer -> IO ()
close Nothing = pure ()
close (Just (Timer functions ids ref)) = mask_ $ do
  (_,active,_) <- readIORef ref
  when (active /= Nothing) (GL.endQuery functions 0x88bf)
  withArray ids (GL.deleteQueries functions 4)

sample :: Maybe Timer -> IO Double
sample Nothing = pure (-1)
sample (Just (Timer functions ids ref)) = do
  (pending,active,cursor) <- readIORef ref
  case pending of
    [] -> pure (-1)
    first:rest -> case drop first ids of
      query:_ -> alloca $ \done -> do
        GL.queryAvailable functions query 0x8867 done
        available <- peek done :: IO CInt
        if available == 0 then pure (-1) else alloca $ \result -> do
          GL.queryResult functions query 0x8866 result
          ns <- peek result
          writeIORef ref (rest,active,cursor)
          pure (fromIntegral ns / 1000000)
      [] -> pure (-1)

begin :: Maybe Timer -> IO ()
begin Nothing = pure ()
begin (Just (Timer functions ids ref)) = do
  (pending,active,cursor) <- readIORef ref
  when (active == Nothing && cursor `notElem` pending) $ case drop cursor ids of
    query:_ -> do
      RL.rlDrawRenderBatchActive
      GL.beginQuery functions 0x88bf query
      writeIORef ref (pending,Just cursor,cursor)
    [] -> pure ()

end :: Maybe Timer -> IO ()
end Nothing = pure ()
end (Just (Timer functions _ ref)) = do
  (pending,active,cursor) <- readIORef ref
  case active of
    Nothing -> pure ()
    Just slot -> do
      RL.rlDrawRenderBatchActive
      GL.endQuery functions 0x88bf
      writeIORef ref (pending++[slot],Nothing,(cursor+1) `mod` 4)

withGraphics :: (Graphics -> IO a) -> IO a
withGraphics action = bracket open close $ \timer -> do
  state <- newIORef (Nothing, initialGovernor)
  counts <- newIORef (0, 0)
  action (Graphics timer state counts)

choose :: Graphics -> GraphicsQuality -> RestScene -> Int -> Int -> Bool -> IO Tier
choose (Graphics timer state counts) preference scene w h adaptive = do
  (oldKey, old) <- readIORef state
  let key = (preference, scene, w, h)
      same = oldKey == Just key
      current = if same then old else initialGovernor
  ms <- sample timer
  let updated = if same && adaptive && preference == Automatic then observe ms current else current
  if adaptive && preference == Automatic && ms > 0 && same
    then modifyIORef' counts (\(n, _) -> (n + 1, ms))
    else pure ()
  -- Previews don't replace the full-size context. The settling interval also
  -- excludes any query submitted before a preference/viewport transition.
  writeIORef state (Just key, updated)
  pure (maybe (if adaptive || same then tier updated else Balanced) id (fixedTier preference))

measured :: Graphics -> Bool -> IO a -> IO a
measured (Graphics timer _ _) enabled = if enabled then bracket_ (begin timer) (end timer) else id

diagnostics :: Graphics -> IO (Bool, Tier, Int, Double)
diagnostics (Graphics timer state counts) = do
  (_, g) <- readIORef state
  (n, ms) <- readIORef counts
  pure (case timer of Nothing -> False; Just _ -> True, tier g, n, ms)
