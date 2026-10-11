{-# LANGUAGE PatternSynonyms #-}

-- CPU decoding runs on a worker; only texture upload touches the GL thread.
module Stillroom.Runtime.Pictures
  ( PictureEntry (..), PictureOrigin (..), PictureError (..), PreparedPicture,
    discoverPictures, findPicture, adjacentPicture, importPicture, preparePicture,
    uploadPicture, releasePicture, removePicture, pictureErrorText, assetPath,
    setAppIcon,
  ) where

import Control.Exception hiding (handle)
import Control.Monad (filterM, unless, when)
import Data.ByteString qualified as BS
import Data.Char (toLower)
import Data.List (find, findIndex, sort)
import Foreign (ForeignPtr, Ptr, castPtr, finalizeForeignPtr, free, nullPtr, peek, withForeignPtr)
import Foreign.C (withCString)
import Foreign.Concurrent qualified as FC
import Paths_stillroom (getDataFileName)
import Raylib.Core (c'setWindowIcon)
import Raylib.Core.Textures
import Raylib.Internal (WindowResources, managed)
import Raylib.Internal.Foreign (c'free, pop)
import Raylib.Types
import Stillroom.Domain
import System.Directory
import System.Environment (getExecutablePath)
import System.FilePath
import System.IO (IOMode (ReadMode), hClose, openBinaryTempFile, withBinaryFile)

data PictureOrigin = Bundled | Imported deriving (Eq, Show)
data PictureEntry = PictureEntry
  { pictureKey :: !String, picturePath :: !FilePath, pictureName :: !String,
    pictureOrigin :: !PictureOrigin
  } deriving (Eq, Show)

data PictureError = ImageUnavailable | UnsupportedImage | ImageTooLarge | ImageDecodeFailed
  deriving (Eq, Show)
instance Exception PictureError
newtype PreparedPicture = PreparedPicture (ForeignPtr Image)

pictureErrorText :: Language -> PictureError -> String
pictureErrorText Japanese ImageUnavailable = "画像を読み込めません。現在の背景を続けます。"
pictureErrorText Japanese UnsupportedImage = "PNG・JPEG・BMP の画像を選んでください。"
pictureErrorText Japanese ImageTooLarge = "画像が大きすぎます。32 MB・8192 px・3200万画素までです。"
pictureErrorText Japanese ImageDecodeFailed = "この画像は表示できません。現在の背景を続けます。"
pictureErrorText English ImageUnavailable = "Image unavailable; the current background continues."
pictureErrorText English UnsupportedImage = "Choose a PNG, JPEG or BMP image."
pictureErrorText English ImageTooLarge = "Image limit: 32 MB, 8192 px per edge, 32 megapixels."
pictureErrorText English ImageDecodeFailed = "Cannot decode this image; the current background continues."

assetPath :: FilePath -> IO FilePath
assetPath relative = do
  executable <- getExecutablePath
  let beside = takeDirectory executable </> relative
  exists <- doesPathExist beside
  if exists then pure beside else getDataFileName relative

supported :: FilePath -> Bool
supported file = map toLower (takeExtension file) `elem` [".png", ".jpg", ".jpeg", ".bmp"]

discoverPictures :: FilePath -> IO [PictureEntry]
discoverPictures directory = do
  builtins <- mapM bundled ["01-mist-lake.png", "02-quiet-forest.png", "03-soft-dunes.png", "04-night-field.png"]
  available <- filterM (doesFileExist . picturePath) builtins
  imported <- catch (do
    createDirectoryIfMissing True directory
    names <- sort . filter supported <$> listDirectory directory
    files <- filterM (doesFileExist . (directory </>)) names
    pure [PictureEntry ("user:" ++ name) (directory </> name) (takeBaseName name) Imported | name <- files])
    (\(_ :: IOException) -> pure [])
  pure (available ++ imported)
  where
    bundled name = do
      path <- assetPath ("assets" </> "backgrounds" </> name)
      pure (PictureEntry ("bundled:" ++ name) path (takeBaseName name) Bundled)

findPicture :: Maybe String -> [PictureEntry] -> Maybe PictureEntry
findPicture key entries = case key >>= (\k -> find ((== k) . pictureKey) entries) of
  Just entry -> Just entry
  Nothing -> case entries of [] -> Nothing; entry : _ -> Just entry

adjacentPicture :: PictureDirection -> Maybe String -> [PictureEntry] -> Maybe PictureEntry
adjacentPicture _ _ [] = Nothing
adjacentPicture direction key entries =
  let index = key >>= (\k -> findIndex ((== k) . pictureKey) entries)
      delta = case direction of PreviousPicture -> -1; NextPicture -> 1
      target = maybe 0 (\i -> (i + delta) `mod` length entries) index
   in case drop target entries of [] -> Nothing; entry : _ -> Just entry

attempt :: IO a -> IO (Either PictureError a)
attempt action = catch (try action) (\(_ :: IOException) -> pure (Left ImageUnavailable))

readImageBytes :: FilePath -> IO BS.ByteString
readImageBytes file = do
  unless (supported file) (throwIO UnsupportedImage)
  size <- getFileSize file
  when (size > 32 * 1024 * 1024) (throwIO ImageTooLarge)
  -- A bounded read also handles a file growing after the size check.
  bytes <- withBinaryFile file ReadMode (\handle -> BS.hGet handle (32 * 1024 * 1024 + 1))
  when (BS.length bytes > 32 * 1024 * 1024) (throwIO ImageTooLarge)
  pure bytes

prepareBytes :: FilePath -> BS.ByteString -> IO PreparedPicture
prepareBytes file bytes = mask $ \restore -> do
  raw <- BS.useAsCStringLen bytes $ \(buffer, size) ->
    withCString (map toLower (takeExtension file)) $ \extension ->
      c'loadImageFromMemory extension (castPtr buffer) (fromIntegral size)
  when (raw == nullPtr) (throwIO ImageDecodeFailed)
  owned <- FC.newForeignPtr raw (freeImage raw)
  restore (do
    valid <- c'isImageValid raw
    unless (valid /= 0) (throwIO ImageDecodeFailed)
    width <- peek (p'image'width raw)
    height <- peek (p'image'height raw)
    when (width > 8192 || height > 8192 || toInteger width * toInteger height > 32000000) (throwIO ImageTooLarge)
    let scale = min 1 (3840 / fromIntegral (max width height) :: Double)
    when (scale < 1) $ c'imageResize raw (max 1 (round (fromIntegral width * scale))) (max 1 (round (fromIntegral height * scale)))
    pure (PreparedPicture owned)) `onException` finalizeForeignPtr owned
  where
    -- h-raylib 5.6 stores decoded pixels as a linked Haskell byte list when
    -- using pop Image. Keep native ownership here to avoid that large copy.
    freeImage :: Ptr Image -> IO ()
    freeImage raw = (c'free . castPtr =<< peek (p'image'data raw)) `finally` free raw

preparePicture :: PictureEntry -> IO (Either PictureError PreparedPicture)
preparePicture entry = attempt (readImageBytes (picturePath entry) >>= prepareBytes (picturePath entry))

importPicture :: FilePath -> FilePath -> IO (Either PictureError (PictureEntry, PreparedPicture))
importPicture directory source = attempt $ do
  bytes <- readImageBytes source
  prepared <- prepareBytes source bytes
  flip onException (releasePicture prepared) $ do
    createDirectoryIfMissing True directory
    -- The exact bytes just validated are copied; the original is never owned.
    file <- bracketOnError
      (openBinaryTempFile directory (takeBaseName source ++ map toLower (takeExtension source)))
      (\(path, handle) -> hClose handle `finally` removeFile path)
      (\(path, handle) -> BS.hPut handle bytes >> hClose handle >> pure path)
    pure (PictureEntry ("user:" ++ takeFileName file) file (takeBaseName file) Imported, prepared)

uploadPicture :: WindowResources -> PreparedPicture -> IO Texture
uploadPicture window prepared@(PreparedPicture owned) = flip finally (releasePicture prepared) $
  withForeignPtr owned $ \raw -> do
    texture <- managed window (c'loadTextureFromImage raw >>= pop)
    valid <- isTextureValid texture
    unless valid (throwIO ImageDecodeFailed)
    _ <- setTextureFilter texture TextureFilterBilinear
    pure texture

releasePicture :: PreparedPicture -> IO ()
releasePicture (PreparedPicture owned) = finalizeForeignPtr owned

removePicture :: PictureEntry -> IO (Either PictureError ())
removePicture entry = case pictureOrigin entry of
  Bundled -> pure (Right ())
  Imported -> attempt (removeFile (picturePath entry))

setAppIcon :: IO ()
setAppIcon = do
  file <- assetPath ("assets" </> "icon" </> "stillroom.png")
  result <- attempt (readImageBytes file >>= prepareBytes file)
  case result of
    Left _ -> pure ()
    Right prepared@(PreparedPicture owned) ->
      withForeignPtr owned c'setWindowIcon `finally` releasePicture prepared
