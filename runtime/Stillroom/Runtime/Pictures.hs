{-# LANGUAGE BangPatterns #-}

-- | CPU decoding belongs to the picture worker. The renderer uploads and owns
-- only the completed immutable RGBA image.
module Stillroom.Runtime.Pictures
  ( PictureEntry (..), PictureOrigin (..), PictureError (..), PreparedPicture,
    PictureTexture (..),
    discoverPictures, findPicture, adjacentPicture, importPicture, preparePicture,
    uploadPicture, unloadPicture, removePicture, pictureErrorText, assetPath,
  ) where

import Control.Exception hiding (handle)
import Control.Monad (filterM, unless, when)
import Data.ByteString qualified as BS
import Data.ByteString.Internal qualified as BSI
import Data.ByteString.Unsafe qualified as BSU
import Data.Char (toLower)
import Data.List (find, findIndex, sort)
import Data.Vector qualified as V
import Data.Word (Word8)
import Foreign.Storable (pokeByteOff)
import GameIO.Graphics qualified as GPU
import GameIO.Image qualified as Image
import Stillroom.Domain
import Stillroom.Runtime.Assets (assetPath)
import System.Directory
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
newtype PreparedPicture = PreparedPicture Image.Image
data PictureTexture = PictureTexture !Int !Int !GPU.Texture

pictureErrorText :: Language -> PictureError -> String
pictureErrorText Japanese ImageUnavailable = "画像を読み込めません。現在の背景を続けます。"
pictureErrorText Japanese UnsupportedImage = "PNG・JPEG・BMP の画像を選んでください。"
pictureErrorText Japanese ImageTooLarge = "画像が大きすぎます。32 MB・8192 px・3200万画素までです。"
pictureErrorText Japanese ImageDecodeFailed = "この画像は表示できません。現在の背景を続けます。"
pictureErrorText English ImageUnavailable = "Image unavailable; the current background continues."
pictureErrorText English UnsupportedImage = "Choose a PNG, JPEG or BMP image."
pictureErrorText English ImageTooLarge = "Image limit: 32 MB, 8192 px per edge, 32 megapixels."
pictureErrorText English ImageDecodeFailed = "Cannot decode this image; the current background continues."

supported :: FilePath -> Bool
supported file = map toLower (takeExtension file) `elem` [".png", ".jpg", ".jpeg", ".bmp"]

discoverPictures :: FilePath -> IO [PictureEntry]
discoverPictures directory = do
  builtins <- mapM bundled
    [ "open-water.png", "first-light.png", "honey-mist.png", "blue-distance.png",
      "01-mist-lake.png", "02-quiet-forest.png", "03-soft-dunes.png",
      "04-night-field.png", "05-afternoon-lake.png", "06-woodland-pond.png"
    ]
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

prepareBytes :: BS.ByteString -> IO PreparedPicture
prepareBytes bytes =
  either (throwIO . pictureError) (pure . PreparedPicture) (Image.decodeImage bytes >>= limitPicture)
  where
    pictureError (Image.ImageDimensions width height)
      | width > 8192 || height > 8192 || toInteger width * toInteger height > 32000000 = ImageTooLarge
    pictureError _ = ImageDecodeFailed

-- Bilinear resampling is bounded by the original preview texture limit. A
-- single RGBA buffer replaces the decoded one when an edge exceeds 3840.
limitPicture :: Image.Image -> Either Image.ImageError Image.Image
limitPicture image
  | extent <= 3840 = Right image
  | otherwise = Image.rgbaImage newWidth newHeight pixels
  where
    (width, height) = Image.imageSize image
    extent = max width height
    newWidth = max 1 (div (width * 3840 + div extent 2) extent)
    newHeight = max 1 (div (height * 3840 + div extent 2) extent)
    source = Image.imageRGBA image
    fraction = 256 :: Int
    horizontal = V.generate newWidth (axis width newWidth)
    axis oldSize newSize sample =
      let numerator = (2 * sample + 1) * oldSize - newSize
          (base, offset) = divMod numerator (2 * newSize)
          blend = div (offset * fraction) (2 * newSize)
      in (base, min (oldSize - 1) (base + 1), blend)
    pixels = BSI.unsafeCreate (newWidth * newHeight * 4) $ \destination -> do
      let rows !y
            | y >= newHeight = pure ()
            | otherwise = do
                let (y0, y1, fy) = axis height newHeight y
                    columns !x
                      | x >= newWidth = pure ()
                      | otherwise = do
                          let (x0, x1, fx) = V.unsafeIndex horizontal x
                              byte px py channel = fromIntegral (BSU.unsafeIndex source (4 * (py * width + px) + channel))
                              linear a b t = a * (fraction - t) + b * t
                              channelValue channel =
                                let top = linear (byte x0 y0 channel) (byte x1 y0 channel) fx
                                    bottom = linear (byte x0 y1 channel) (byte x1 y1 channel) fx
                                in fromIntegral (div (linear top bottom fy + 32768) 65536)
                              offset = 4 * (y * newWidth + x)
                              channels !channel
                                | channel == 4 = pure ()
                                | otherwise = pokeByteOff destination (offset + channel) (channelValue channel :: Word8) >> channels (channel + 1)
                          channels 0
                          columns (x + 1)
                columns 0
                rows (y + 1)
      rows 0

preparePicture :: PictureEntry -> IO (Either PictureError PreparedPicture)
preparePicture entry = attempt (readImageBytes (picturePath entry) >>= prepareBytes)

importPicture :: FilePath -> FilePath -> IO (Either PictureError (PictureEntry, PreparedPicture))
importPicture directory source = attempt $ do
  bytes <- readImageBytes source
  prepared <- prepareBytes bytes
  createDirectoryIfMissing True directory
  -- Copy the exact bytes that passed validation. The original is borrowed.
  file <- bracketOnError
    (openBinaryTempFile directory (takeBaseName source ++ map toLower (takeExtension source)))
    (\(path, handle) -> finally (hClose handle) (removeFile path))
    (\(path, handle) -> BS.hPut handle bytes >> hClose handle >> pure path)
  pure (PictureEntry ("user:" ++ takeFileName file) file (takeBaseName file) Imported, prepared)

uploadPicture :: GPU.Renderer -> PreparedPicture -> IO PictureTexture
uploadPicture renderer (PreparedPicture image) = do
  let (width, height) = Image.imageSize image
  texture <- catch (GPU.createTextureRGBA8With renderer GPU.Linear width height (Image.imageRGBA image)) $ \issue ->
    case issue of
      GPU.InvalidTexture _ _ _ -> throwIO ImageDecodeFailed
      other -> throwIO other
  pure (PictureTexture width height texture)

unloadPicture :: GPU.Renderer -> PictureTexture -> IO ()
unloadPicture renderer (PictureTexture _ _ texture) = GPU.destroyTexture renderer texture

removePicture :: PictureEntry -> IO (Either PictureError ())
removePicture entry = case pictureOrigin entry of
  Bundled -> pure (Right ())
  Imported -> attempt (removeFile (picturePath entry))
