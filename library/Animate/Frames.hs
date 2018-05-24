module Animate.Frames where

import qualified Data.Map as Map
import Data.Map (Map)
import Data.Text (pack)
import Codec.Picture
import Safe (headMay)
import Data.Maybe (fromMaybe)
import Data.List (find)
import GHC.Float (sqrtFloat)
import Animate (FrameIndex, SpriteSheetInfo(..))

import Animate.Frames.Options (getOptions, printUsage, Options(..))

type Seconds = Float

data Layout = Layout
  { layoutSize :: (Int, Int)
  , layoutRows :: [Row]
  , layoutAnimations :: Map String [(FrameIndex, Seconds)]
  }

data CropInfo = CropInfo
  { cInfoAnimations :: Map String [CropFrame]
  , cInfoImages :: Map CropId CropImage
  }
  
data Row = Row
  { rowCropImages :: [CropImage]
  , rowTop :: Int
  , rowHeight :: Int
  , rowWidth :: Int
  }

data RowStep = RowStep
  { rsCurrent :: Row
  , rsFinished :: [Row]
  }

data CropImage = CropImage
  { ciImage :: Image PixelRGBA8
  , ciCoords :: ((Int, Int), (Int, Int))
  , ciOrigin :: (Int, Int)
  , ciDim :: (Int, Int)
  }

instance Eq CropImage where
  a == b = and
    [ eqImagePixelRGBA8 (ciImage a) (ciImage b)
    , ciCoords a == ciCoords b
    , ciOrigin a == ciOrigin b
    , ciDim a == ciDim b
    ]

type CropId = Int

data CropFrame = CropFrame
  { cfCropId :: CropId
  , cfCount :: Int -- Number of sequental and equvialent frames compressed as one
  } deriving (Show, Eq)

data Tree a
  = Leaf a
  | Node a (Maybe (Tree a)) (Maybe (Tree a))
  deriving (Show, Eq)

data Range = Range
  { rMin :: Int
  , rMax :: Int
  } deriving (Show, Eq)

data HorzNode = HorzNode
  { hnRange :: Range
  , hnCropImage :: CropImage
  }

data VertNode = VertNode
  { vnRange :: Range
  , vnHorzTree :: Tree HorzNode
  }  
  
--

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
        return (key, map mkCropImage images)

      flip mapM_ animations' $ \(key,images) -> do
        print key
        mapM_ (\CropImage{ciCoords,ciOrigin,ciDim} -> print (ciCoords,ciOrigin,ciDim)) images

--

readImageOrFail :: FilePath -> IO DynamicImage
readImageOrFail fp = do
  img' <- readImage fp
  case img' of
    Left _ -> fail $ "Can't load image: " ++ fp
    Right img -> return img

layoutToSpriteSheetInfo :: FilePath -> Layout -> SpriteSheetInfo String Seconds
layoutToSpriteSheetInfo fp layout = SpriteSheetInfo
  { ssiImage = fp
  , ssiAlpha = Nothing
  , ssiClips = []
  , ssiAnimations = Map.mapKeys pack (layoutAnimations layout)
  }

layoutCrops :: Int -> Map String [CropImage] -> Layout
layoutCrops fps cropImages = Layout size rows animations
  where
    size = (0,0)
    rows = []
    animations = cropAnimationsToLayoutAnimations fps (cInfoAnimations cropInfo)
    cropInfo = buildCropInfo cropImages

inRange :: Int -> Range -> Bool
inRange x r = x >= rMin r && x < rMax r

lessThanRange :: Int -> Range -> Bool
lessThanRange x r = x < rMin r

greaterThanRange :: Int -> Range -> Bool
greaterThanRange x r = x < rMax r

lookupNodeWithinRange :: (n -> Range) -> Tree n -> Int -> Maybe n
lookupNodeWithinRange toRange tree x = case tree of
  Leaf n -> if inRange x (toRange n)
    then Just n
    else Nothing
  Node n left right -> if inRange x (toRange n)
    then Just n
    else if lessThanRange x (toRange n)
      then left >>= \l -> lookupNodeWithinRange toRange l x
      else right >>= \r -> lookupNodeWithinRange toRange r x

mkRows
  :: (Int, Int) -- Minimum boundaries
  -> [CropImage]
  -> [Row]
mkRows (minX, _) images = rsFinished done ++ [rsCurrent done]
  where
    done :: RowStep
    done = foldr stepRow initRowStep images

    stepRow :: CropImage -> RowStep -> RowStep
    stepRow ci (RowStep cur finished) = let
      cur' = appendCropImage cur ci
      in if minX > rowWidth cur'
        then RowStep cur' finished
        else RowStep initRow{ rowTop = 1 + rowTop cur' + rowHeight cur' } (finished ++ [cur'])

appendCropImage :: Row -> CropImage -> Row
appendCropImage row ci = row
  { rowCropImages = rowCropImages row ++ [ci]
  , rowHeight = max (rowHeight row) cropImageHeight
  , rowWidth = rowWidth row + cropImageWidth
  }
  where
    (cropImageWidth, cropImageHeight) = ciDim ci

initRow :: Row
initRow = Row [] 0 0 0

initRowStep :: RowStep
initRowStep = RowStep initRow []

cropAnimationsToLayoutAnimations
  :: Int -- ^ Frames per seconds
  -> Map String [CropFrame] -- ^ Crop animations
  -> Map String [(FrameIndex, Seconds)] 
cropAnimationsToLayoutAnimations fps cropAnimations = fmap
    (map (\CropFrame{cfCount,cfCropId} -> (cfCropId, sum $ replicate cfCount spf)))
    cropAnimations
  where
    spf = 1 / fromIntegral fps

buildCropInfo :: Map String [CropImage] -> CropInfo
buildCropInfo animations = let
  (frames, images) = Map.foldrWithKey build (Map.empty, Map.empty) animations
  in CropInfo frames images
  where
    build
      :: String
      -> [CropImage]
      -> (Map String [CropFrame], Map CropId CropImage)
      -> (Map String [CropFrame], Map CropId CropImage)
    build aniName imgs (cropFrames, cropImages) = let
      (cropImages', cropIds) = insertCropImages imgs cropImages
      cropFrames' = Map.insert aniName (collapseIntoFrames cropIds) cropFrames
      in (cropFrames', cropImages')

insertCropImages :: [CropImage] -> Map CropId CropImage -> (Map CropId CropImage, [CropId])
insertCropImages imgs cropImages = foldr insertCropImagesStep (cropImages, []) imgs

insertCropImagesStep
  :: CropImage
  -> (Map CropId CropImage, [CropId])
  -> (Map CropId CropImage, [CropId])
insertCropImagesStep cropImage (cropImages, cropIds) = let
  (cropImages', cropId) = insertCropImage cropImage cropImages
  in (cropImages',cropIds ++ [cropId])

collapseIntoFrames :: [CropId] -> [CropFrame]
collapseIntoFrames [] = []
collapseIntoFrames (x:xs) = let
  (included, after) = span (== x) xs
  in CropFrame x (1 + length included) : collapseIntoFrames after

eqImagePixelRGBA8 :: Image PixelRGBA8 -> Image PixelRGBA8 -> Bool
eqImagePixelRGBA8 a b =
  imageWidth a == imageWidth b &&
  imageHeight a == imageHeight b &&
  imageData a == imageData b

insertCropImage :: CropImage -> Map CropId CropImage -> (Map CropId CropImage, CropId)
insertCropImage img imgs = case findByElem imgs img of
  Just cropId -> (imgs, cropId)
  Nothing -> let
    cropId = Map.size imgs
    imgs' = Map.insert cropId img imgs
    in (imgs', cropId)

findByElem :: Eq a => Map k a -> a -> Maybe k
findByElem m v = fst <$> find (\(_,w) -> v == w) (Map.toList m)

sumDim :: [CropImage] -> (Int, Int)
sumDim = foldr (\CropImage{ciDim} (w,h) -> (fst ciDim + w, snd ciDim + h)) (0,0)

maxHeight :: [CropImage] -> Int
maxHeight = maximum . map (snd . ciDim)

minBoundaries :: [CropImage] -> (Int, Int)
minBoundaries images = let
  dim = sumDim images
  in ( round . sqrtFloat . fromIntegral . fst $ dim
     , round . sqrtFloat . fromIntegral . snd $ dim
     )

mkCropImage :: DynamicImage -> CropImage
mkCropImage di = CropImage
  { ciImage = img
  , ciCoords = coords
  , ciOrigin = (imageWidth img `div` 2, imageHeight img `div` 2)
  , ciDim = cropImageDim coords
  }
  where
    img = convertRGBA8 di
    coords = cropCoordsImage img

cropImageDim :: ((Int, Int), (Int, Int)) -> (Int, Int)
cropImageDim ((x0,y0), (x1,y1)) = (x1 - x0, y1 - y0)

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