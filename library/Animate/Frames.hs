module Animate.Frames where

import qualified Data.Map as Map
import Codec.Picture
import Codec.Picture.Types (thawImage)
import Safe (headMay)
import Data.Maybe (fromMaybe)

import Animate.Frames.Options (getOptions, printUsage, Options(..))

main :: IO ()
main = do
  options' <- getOptions
  case options' of
    Nothing -> printUsage
    Just options -> do
      print options
      let animations = Map.toList (optionsAnimations options)
      animations' <- flip mapM animations $ \(key,frames) -> do
        images <- mapM readImageOrFail frames
        return (key, map mkCroppedImage images)

      flip mapM_ animations' $ \(key,images) -> do
        print key
        mapM_ (\CroppedImage{ciCoords,ciOrigin,ciDim} -> print (ciCoords,ciOrigin,ciDim)) images

readImageOrFail :: FilePath -> IO DynamicImage
readImageOrFail fp = do
  img' <- readImage fp
  case img' of
    Left _ -> fail $ "Can't load image: " ++ fp
    Right img -> return img

data CroppedImage = CroppedImage
  { ciImage :: Image PixelRGBA8
  , ciCoords :: ((Int, Int), (Int, Int))
  , ciOrigin :: (Int, Int)
  , ciDim :: (Int, Int)
  }

sumDim :: [CroppedImage] -> (Int, Int)
sumDim = foldr (\CroppedImage{ciDim} (w,h) -> (fst ciDim + w, snd ciDim + h)) (0,0)

maxHeight :: [CroppedImage] -> Int
maxHeight = maximum . map (snd . ciDim)

minBoundaries :: [CroppedImage] -> (Int, Int)
minBoundaries images = let
  dim = sumDim images
  in (round . sqrt . fromIntegral . fst $ dim, round . sqrt . fromIntegral . snd $ dim)

mkCroppedImage :: DynamicImage -> CroppedImage
mkCroppedImage di = CroppedImage
  { ciImage = img
  , ciCoords = coords
  , ciOrigin = (imageWidth img `div` 2, imageHeight img `div` 2)
  , ciDim = croppedImageDim coords
  }
  where
    img = convertRGBA8 di
    coords = cropCoordsImage img

croppedImageDim :: ((Int, Int), (Int, Int)) -> (Int, Int)
croppedImageDim ((x0,y0), (x1,y1)) = (x1 - x0, y1 - y0)

cropCoordsImage :: (Pixel a, Eq (PixelBaseComponent a)) => Image a -> ((Int, Int), (Int, Int))
cropCoordsImage img = fromMaybe ((0,0), (1,1)) maybeCropped
    where
      maybeCropped = (,)
        <$> ((,) <$> findX0 img <*> findY0 img)
        <*> ((,) <$> findX1 img <*> findY1 img)

firstOpaquePoint :: (Pixel a, Eq (PixelBaseComponent a)) => (Image a -> [(Int, Int)]) -> ((Int, Int) -> Int) -> Image a -> Maybe Int
firstOpaquePoint mkCoords whichPoint img = fmap fst $ headMay $ filter snd (map getPixel coords)
  where
    getPixel coord@(x,y) = (whichPoint coord, pixelOpacity (pixelAt img x y) == 255)
    coords = mkCoords img

findY0 :: (Pixel a, Eq (PixelBaseComponent a)) => Image a -> Maybe Int
findY0 = firstOpaquePoint topDown snd

findY1 :: (Pixel a, Eq (PixelBaseComponent a)) => Image a -> Maybe Int
findY1 = firstOpaquePoint downTop snd

findX0 :: (Pixel a, Eq (PixelBaseComponent a)) => Image a -> Maybe Int
findX0 = firstOpaquePoint leftRight fst

findX1 :: (Pixel a, Eq (PixelBaseComponent a)) => Image a -> Maybe Int
findX1 = firstOpaquePoint rightLeft fst

topDown :: Image a -> [(Int,Int)]
topDown Image{imageWidth,imageHeight} = [(x,y) | y <- [0..pred imageHeight], x <- [0..pred imageWidth]]

downTop :: Image a -> [(Int, Int)]
downTop Image{imageWidth,imageHeight} = [(x,y) | y <- reverse [0..pred imageHeight], x <- [0..pred imageWidth]]

leftRight :: Image a -> [(Int, Int)]
leftRight Image{imageWidth,imageHeight} = [(x,y) | x <- [0..pred imageWidth], y <- [0..pred imageHeight]]

rightLeft :: Image a -> [(Int, Int)]
rightLeft Image{imageWidth,imageHeight} = [(x,y) | x <- reverse [0..pred imageWidth], y <- [0..pred imageHeight]]