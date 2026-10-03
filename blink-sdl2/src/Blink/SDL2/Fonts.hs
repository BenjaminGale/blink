-- | Loads the SDL font for each Blink 'Font' a frame uses, once per
-- family, size and weight.
module Blink.SDL2.Fonts
  ( FontFile (..)
  , FontCache
  , newFontCache
  , freeFontCache
  , loadFont
  , checkFontFiles
  ) where

import Blink.Backend (Font (..), FontWeight (..))
import Control.Exception (SomeException, throwIO, try)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified SDL.Font as SDLFont

-- | A TrueType file providing one family at one weight.
data FontFile = FontFile
  { fontFileFamily :: Text
  , fontFileWeight :: FontWeight
  , fontFilePath   :: FilePath
  }

-- | The configured files and every SDL font loaded from them so far.
data FontCache = FontCache
  { fcFiles  :: NonEmpty FontFile
  , fcLoaded :: IORef (Map (Text, Int, FontWeight) SDLFont.Font)
  }

newFontCache :: NonEmpty FontFile -> IO FontCache
newFontCache files = FontCache files <$> newIORef Map.empty

freeFontCache :: FontCache -> IO ()
freeFontCache cache = readIORef (fcLoaded cache) >>= mapM_ SDLFont.free

-- | The SDL font for @font@. An unknown family uses the first file's
-- family. A weight with no file of its own is drawn from another file of
-- the same family, made bold by SDL_ttf when bold was asked for.
loadFont :: FontCache -> Font -> IO SDLFont.Font
loadFont cache font = do
  loaded <- readIORef (fcLoaded cache)
  case Map.lookup cacheKey loaded of
    Just sdlFont -> pure sdlFont
    Nothing      -> do
      sdlFont <- SDLFont.load (fontFilePath file) points
      SDLFont.setStyle sdlFont [SDLFont.Bold | weight == Bold && fontFileWeight file /= Bold]
      modifyIORef' (fcLoaded cache) (Map.insert cacheKey sdlFont)
      pure sdlFont
  where
    files         = fcFiles cache
    defaultFamily = fontFileFamily (NE.head files)
    known f       = any ((== f) . fontFileFamily) files
    family        = case fontFamily font of
      Just f | known f -> f
      _                -> defaultFamily
    weight        = fontWeight font
    points        = max 1 (round (fontSize font))
    cacheKey      = (family, points, weight)
    inFamily      = NE.filter ((== family) . fontFileFamily) files
    file          = case filter ((== weight) . fontFileWeight) inFamily ++ inFamily of
      (f : _) -> f
      []      -> NE.head files

-- | Opens and closes every file, failing with an error that names the
-- first file SDL_ttf can't load.
checkFontFiles :: NonEmpty FontFile -> IO ()
checkFontFiles = mapM_ check
  where
    check file = do
      result <- try (SDLFont.load (fontFilePath file) 12) :: IO (Either SomeException SDLFont.Font)
      case result of
        Right sdlFont -> SDLFont.free sdlFont
        Left err      -> throwIO (userError ("could not load font file " ++ show (fontFilePath file) ++ ": " ++ show err))
