module Animate.Frames where

import qualified Data.Map as Map
import Codec.Picture
import Codec.Picture.Types (thawImage)
import Safe (headMay)

import Animate.Frames.Options (getOptions, printUsage, Options(..))

main :: IO ()
main = do
  options' <- getOptions
  case options' of
    Nothing -> printUsage
    Just options -> do
      print options
      let animations = Map.toList (optionsAnimations options)
      flip mapM animations $ \(key,frames) -> do
        print key
        images <- mapM readImage frames
        images' <- flip mapM (zip frames images) $ \(frame,image) -> case image of
          Left _ -> fail $ "Can't load frame"
          Right image' -> return (frame, image')
        mapM_ print $ flip map images' $ \(f,i) -> f ++ ": " ++ show (cropCoords i)
      return ()

cropCoords :: DynamicImage -> Maybe ((Int, Int), (Int, Int))
cropCoords di = case di of
  ImageY8 i -> cropCoordsImage i
  ImageY16 i -> cropCoordsImage i
  ImageYF i -> cropCoordsImage i
  ImageYA8 i -> cropCoordsImage i
  ImageYA16 i -> cropCoordsImage i
  ImageRGB8 i -> cropCoordsImage i
  ImageRGB16 i -> cropCoordsImage i
  ImageRGBF i -> cropCoordsImage i
  ImageRGBA8 i -> cropCoordsImage i
  ImageRGBA16 i -> cropCoordsImage i
  ImageYCbCr8 i -> cropCoordsImage i
  ImageCMYK8 i -> cropCoordsImage i
  ImageCMYK16 i -> cropCoordsImage i

cropCoordsImage :: (Pixel a, Eq (PixelBaseComponent a)) => Image a -> Maybe ((Int, Int), (Int, Int))
cropCoordsImage img = (,)
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