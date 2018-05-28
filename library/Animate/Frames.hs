module Animate.Frames where

import qualified Data.Map as Map
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text.IO as T
import qualified Data.Text as T
import qualified Data.Aeson as A
import Control.Monad (forM)
import Control.Concurrent.Async
import Control.Concurrent.STM
import Data.Digest.Pure.MD5
import Text.Printf (printf)
import Data.Map (Map)
import Data.Monoid ((<>))
import Data.Text (pack, Text)
import Codec.Picture
import Codec.Picture.Png (encodePng)
import Safe (headMay)
import Data.Maybe (fromMaybe)
import Data.List (find, sortBy, foldl')
import GHC.Float (sqrtFloat)
import Animate (FrameIndex, SpriteSheetInfo(..), SpriteClip(..))

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
  { ciImage :: ImageId
  , ciCoords :: ((Int, Int), (Int, Int))
  , ciOrigin :: (Int, Int)
  , ciDim :: (Int, Int)
  }

instance Eq CropImage where
  a == b = and
    [ ciImage a == ciImage b
    , ciCoords a == ciCoords b
    , ciOrigin a == ciOrigin b
    , ciDim a == ciDim b
    ]

type CropId = Int

newtype ImageId = ImageId MD5Digest
  deriving (Show, Eq, Ord)

data CropFrame = CropFrame
  { cfCropId :: CropId
  , cfCount :: Int -- Number of sequental and equvialent frames compressed as one
  } deriving (Show, Eq)

data Tree a = Node a (Maybe (Tree a)) (Maybe (Tree a))
  deriving (Show, Eq)

instance Functor Tree where
  fmap f (Node a ml mr) = Node (f a) (fmap f <$> ml) (fmap f <$> mr)

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
    Just options -> 
      if validAnimationCount options
        then do
          let animations = Map.toList (optionsAnimations options)
          animationImages <- forConcurrently animations $ \(animationKey,frames) -> do
            imageIdsAndImages <- mapM readImageOrFail frames
            return (animationKey, imageIdsAndImages)
          let imageMap = Map.fromList $ concatMap snd animationImages
          animations' <- createCropImagesWithCache animationImages imageMap
          let layout = layoutCrops (optionsFps options) animations'
          let spriteSheetInfo = layoutToSpriteSheetInfo (optionsSpritesheet options) layout
          let image = generateImageFromLayout imageMap layout
          BL.writeFile (optionsSpritesheet options) (encodePng image)
          if optionsYaml options
            then T.writeFile (optionsMetadata options) (customWriteSpriteSheetInfo spriteSheetInfo)
            else BL.writeFile (optionsMetadata options) (A.encode spriteSheetInfo)
        else do
          putStrLn "Not enough animation frame images"
          printUsage

createCropImagesWithCache :: [(String, [(ImageId, a)])] -> Map ImageId (Image PixelRGBA8) -> IO (Map String [CropImage])
createCropImagesWithCache animationImages imageMap = do
  imageIdCropImageMapTVar <- newTVarIO Map.empty
  animationImages' <- forM animationImages $ \(animationKey, imageIdsAndImages) -> do
    xs <- forConcurrently imageIdsAndImages $ \(imageId, _) -> do
      imageIdCropImageMap <- readTVarIO imageIdCropImageMapTVar
      case Map.lookup imageId imageIdCropImageMap of
        Nothing -> do
          let cropImage = mkCropImage imageMap imageId
          atomically $ modifyTVar imageIdCropImageMapTVar $ \m -> Map.insert imageId cropImage m
          return cropImage
        Just cropImage -> return cropImage
    return (animationKey, xs)   
  return $ Map.fromList animationImages'
--

validAnimationCount :: Options -> Bool
validAnimationCount options = not $ any null $ Map.elems (optionsAnimations options)

--

writeCropImage :: Map ImageId (Image PixelRGBA8) -> FilePath -> CropImage -> IO ()
writeCropImage images fp ci = BL.writeFile fp (encodePng $ generateImageFromCropImage images ci)

generateImageFromCropImage :: Map ImageId (Image PixelRGBA8) -> CropImage -> Image PixelRGBA8
generateImageFromCropImage images ci = generateImage genPixel w h
  where
    (w, h) = ciDim ci
    ((ofsX, ofsY), _) = ciCoords ci
    img = images Map.! (ciImage ci)
    genPixel x y = pixelAt img (x + ofsX) (y + ofsY)

readImageOrFail :: FilePath -> IO (ImageId, Image PixelRGBA8)
readImageOrFail fp = do
  bytes <- BL.readFile fp
  let digest = md5 bytes
  let img' = decodeImage (BL.toStrict bytes)
  case img' of
    Left _ -> fail $ "Can't load image: " ++ fp
    Right img -> return (ImageId digest, convertRGBA8 img)

layoutToSpriteSheetInfo :: FilePath -> Layout -> SpriteSheetInfo String Seconds
layoutToSpriteSheetInfo fp layout = SpriteSheetInfo
  { ssiImage = fp
  , ssiAlpha = Nothing
  , ssiClips = spriteClipsFromRows (layoutRows layout)
  , ssiAnimations = Map.mapKeys pack (layoutAnimations layout)
  }

customWriteSpriteSheetInfo :: SpriteSheetInfo a Seconds -> Text
customWriteSpriteSheetInfo ssi = T.unlines (linesOfSpriteSheetInfo ssi)

linesOfSpriteSheetInfo :: SpriteSheetInfo a Seconds -> [Text]
linesOfSpriteSheetInfo ssi =
  ["image: \"" <> pack (ssiImage ssi) <> "\""] ++
  ["alpha: null"] ++
  [] ++
  ["clips:"] ++
  (zipWith spriteClipToText [0..] (ssiClips ssi)) ++
  [] ++
  ["animations:"] ++
  concatMap (uncurry animationToText) (Map.toList (ssiAnimations ssi))

spriteClipToText :: Int -> SpriteClip a -> Text
spriteClipToText idx SpriteClip{scX,scY,scW,scH,scOffset} = mconcat $
  [ "- [" ] ++
  ([textShow scX, ", ", textShow scY, ", ", textShow scW, ", ", textShow scH] <> case scOffset of
      Nothing -> []
      Just (x,y) -> [", ", textShow x, ", ", textShow y]) ++
  ["] # " <> textShow idx]

animationToText :: Text -> [(FrameIndex, Seconds)] -> [Text]
animationToText name frames =
  ["  " <> textShow name <> ":"] ++
  map (\(frameIndex, seconds) -> mconcat ["  - [", textShow frameIndex, ", ", showFloat seconds, "]"]) frames

showFloat :: Float -> Text
showFloat f = pack $ printf "%.4f" f

textShow :: Show a => a -> Text
textShow = pack . show

spriteClipsFromRows :: [Row] -> [SpriteClip String]
spriteClipsFromRows = concatMap buildSpriteClips
  where
    buildSpriteClips :: Row -> [SpriteClip String]
    buildSpriteClips row = fst $ foldr stepSpriteClips ([], 0) (rowCropImages row)
      where
        stepSpriteClips :: CropImage -> ([SpriteClip String], Int) -> ([SpriteClip String], Int)
        stepSpriteClips ci (scs, widthTotal) = (scs ++ [sc], widthTotal + w)
          where
            (w, h) = ciDim ci
            ((ofsX, ofsY), _) = ciCoords ci
            (orgX, orgY) = ciOrigin ci
            sc = SpriteClip
              { scX = widthTotal
              , scY = rowTop row
              , scW = w
              , scH = h
              , scOffset = Just (orgX - ofsX, orgY - ofsY)
              }

generatePixelFromLayout :: Map ImageId (Image PixelRGBA8) -> Layout -> Int -> Int -> PixelRGBA8
generatePixelFromLayout images layout x y = fromMaybe (PixelRGBA8 0 0 0 0) (getPixel x y)
  where
    tree = buildVertTree (layoutRows layout)
    getPixel = lookupPixelFromTree images tree

generateImageFromLayout :: Map ImageId (Image PixelRGBA8) -> Layout -> Image PixelRGBA8
generateImageFromLayout images layout = generateImage (generatePixelFromLayout images layout) w h
  where
    (w, h) = layoutSize layout

layoutCrops :: Int -> Map String [CropImage] -> Layout
layoutCrops fps cropImages = Layout size rows animations
  where
    size = getLayoutDim rows
    boundaries = minBoundaries (Map.elems $ cInfoImages cropInfo)
    rows = mkRows boundaries (map snd . sortByIndex . Map.toList $ cInfoImages cropInfo)
    animations = cropAnimationsToLayoutAnimations fps (cInfoAnimations cropInfo)
    cropInfo = buildCropInfo cropImages

getLayoutDim :: [Row] -> (Int, Int)
getLayoutDim rows = (width, rowTop lastRow + rowHeight lastRow)
  where
    lastRow = last rows
    width = maximum (map rowWidth rows)

sortByIndex :: Ord a => [(a, b)] -> [(a, b)]
sortByIndex = sortBy (\x y -> compare (fst x) (fst y))

inRange :: Int -> Range -> Bool
inRange x r = x >= rMin r && x < rMax r

lessThanRange :: Int -> Range -> Bool
lessThanRange x r = x < rMin r

greaterThanRange :: Int -> Range -> Bool
greaterThanRange x r = x < rMax r

lookupNodeWithinRange :: (n -> Range) -> Tree n -> Int -> Maybe n
lookupNodeWithinRange toRange (Node n left right) v =
  if inRange v (toRange n)
    then Just n
    else if lessThanRange v (toRange n)
      then left  >>= \l -> lookupNodeWithinRange toRange l v
      else right >>= \r -> lookupNodeWithinRange toRange r v

lookupPixelFromTree :: Map ImageId (Image PixelRGBA8) -> Tree VertNode -> Int -> Int -> Maybe PixelRGBA8
lookupPixelFromTree images tree x y = do
  vn <- lookupNodeWithinRange vnRange tree y
  hn <- lookupNodeWithinRange hnRange (vnHorzTree vn) x
  let offset = (rMin (hnRange hn), rMin (vnRange vn))
  pixelFromCropImage images offset (x,y) (hnCropImage hn)

pixelFromCropImage
  :: Map ImageId (Image PixelRGBA8)
  -> (Int, Int) -- ^ Offset
  -> (Int, Int) -- ^ Spritesheet location
  -> CropImage
  -> Maybe PixelRGBA8
pixelFromCropImage images (ofsX,ofsY) (x,y) ci = let
  ((ofsX', ofsY'), _) = ciCoords ci
  (x', y') = (x - ofsX + ofsX', y - ofsY + ofsY')
  img = images Map.! (ciImage ci)
  in if x' >= 0 && y' >= 0 && x' < imageWidth img && y' < imageHeight img
    then Just $ pixelAt img x' y'
    else Nothing

mkRows
  :: (Int, Int) -- Minimum boundaries
  -> [CropImage]
  -> [Row]
mkRows (minX, _) images = rsFinished done ++ (if null . rowCropImages $ rsCurrent done then [] else [rsCurrent done])
  where
    done :: RowStep
    done = foldl' stepRow initRowStep images

    stepRow :: RowStep -> CropImage -> RowStep
    stepRow (RowStep cur finished) ci = let
      cur' = appendCropImage cur ci
      in if minX > rowWidth cur'
        then RowStep cur' finished
        else RowStep initRow{ rowTop = rowTop cur' + rowHeight cur' } (finished ++ [cur'])

appendCropImage :: Row -> CropImage -> Row
appendCropImage row ci = row
  { rowCropImages = ci : rowCropImages row
  , rowHeight = max (rowHeight row) cropImageHeight
  , rowWidth = rowWidth row + cropImageWidth
  }
  where
    (cropImageWidth, cropImageHeight) = ciDim ci

initRow :: Row
initRow = Row [] 0 0 0

initRowStep :: RowStep
initRowStep = RowStep initRow []

buildVertTree :: [Row] -> Tree VertNode
buildVertTree = fmap rowToVertNode . listToTree

rowToVertNode :: Row -> VertNode
rowToVertNode row = VertNode
  { vnRange = Range (rowTop row) (rowTop row + rowHeight row)
  , vnHorzTree = buildHorzTree row
  }

buildHorzTree :: Row -> Tree HorzNode
buildHorzTree = listToTree . fst . foldr stepToHorzNode ([], 0) . rowCropImages
  where
    stepToHorzNode :: CropImage -> ([HorzNode], Int) -> ([HorzNode], Int)
    stepToHorzNode ci (hns, width) = (hns ++ [hn], ciWidth + width)
      where
        hn = HorzNode (Range width (width + ciWidth)) ci
        ciWidth = fst (ciDim ci)

listToTree :: [a] -> Tree a
listToTree [] = error "Can't build VertTree"
listToTree xs = Node m left right
  where
    len = length xs
    mid = div len 2
    (l, m:r) = splitAt mid xs
    left  = if null l then Nothing else Just (listToTree l)
    right = if null r then Nothing else Just (listToTree r)

cropAnimationsToLayoutAnimations
  :: Int -- ^ Frames per seconds
  -> Map String [CropFrame] -- ^ Crop animations
  -> Map String [(FrameIndex, Seconds)] 
cropAnimationsToLayoutAnimations fps = fmap $ map $ \CropFrame{cfCount,cfCropId} ->
  (cfCropId, sum $ replicate cfCount spf)
  where
    spf = 1 / fromIntegral fps

buildCropInfo :: Map String [CropImage] -> CropInfo
buildCropInfo animations = let
  (frames, images) = Map.foldlWithKey' build (Map.empty, Map.empty) animations
  in CropInfo frames images
  where
    build
      :: (Map String [CropFrame], Map CropId CropImage)
      -> String
      -> [CropImage]
      -> (Map String [CropFrame], Map CropId CropImage)
    build (cropFrames, cropImages) aniName imgs = let
      (cropImages', cropIds) = insertCropImages imgs cropImages
      cropFrames' = Map.insert aniName (collapseIntoFrames cropIds) cropFrames
      in (cropFrames', cropImages')

insertCropImages :: [CropImage] -> Map CropId CropImage -> (Map CropId CropImage, [CropId])
insertCropImages imgs cropImages = foldl' insertCropImagesStep (cropImages, []) imgs

insertCropImagesStep
  :: (Map CropId CropImage, [CropId])
  -> CropImage
  -> (Map CropId CropImage, [CropId])
insertCropImagesStep (cropImages, cropIds) cropImage = let
  (cropImages', cropId) = insertCropImage cropImage cropImages
  in (cropImages', cropIds ++ [cropId])

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
  bound = round . ((*) (sqrtFloat num)) . (/num) . fromIntegral
  num = fromIntegral (length images)
  dim = sumDim images
  in ( bound (fst dim), bound (snd dim) )

mkCropImage :: Map ImageId (Image PixelRGBA8) -> ImageId -> CropImage
mkCropImage images imageId = CropImage
  { ciImage = imageId
  , ciCoords = coords
  , ciOrigin = (imageWidth img `div` 2, imageHeight img `div` 2)
  , ciDim = cropImageDim coords
  }
  where
    img = images Map.! imageId
    coords = cropCoordsImage img

cropImageDim :: ((Int, Int), (Int, Int)) -> (Int, Int)
cropImageDim ((x0,y0), (x1,y1)) = (x1 - x0 + 1, y1 - y0 + 1)

cropCoordsImage :: (Pixel a, Eq (PixelBaseComponent a), Ord (PixelBaseComponent a)) => Image a -> ((Int, Int), (Int, Int))
cropCoordsImage img = fromMaybe ((0,0), (1,1)) maybeCropped
    where
      maybeCropped = (,)
        <$> ((,) <$> findX0 img <*> findY0 img)
        <*> ((,) <$> findX1 img <*> findY1 img)

firstOpaquePoint :: (Pixel a, Eq (PixelBaseComponent a), Ord (PixelBaseComponent a)) => (Image a -> [(Int, Int)]) -> ((Int, Int) -> Int) -> Image a -> Maybe Int
firstOpaquePoint mkCoords whichPoint img = fmap fst $ headMay $ filter snd (map getPixel coords)
  where
    getPixel coord@(x,y) = (whichPoint coord, pixelOpacity (pixelAt img x y) > 0)
    coords = mkCoords img

findY0 :: (Pixel a, Eq (PixelBaseComponent a), Ord (PixelBaseComponent a)) => Image a -> Maybe Int
findY0 = firstOpaquePoint topDown snd

findY1 :: (Pixel a, Eq (PixelBaseComponent a), Ord (PixelBaseComponent a)) => Image a -> Maybe Int
findY1 = firstOpaquePoint downTop snd

findX0 :: (Pixel a, Eq (PixelBaseComponent a), Ord (PixelBaseComponent a)) => Image a -> Maybe Int
findX0 = firstOpaquePoint leftRight fst

findX1 :: (Pixel a, Eq (PixelBaseComponent a), Ord (PixelBaseComponent a)) => Image a -> Maybe Int
findX1 = firstOpaquePoint rightLeft fst

topDown :: Image a -> [(Int,Int)]
topDown Image{imageWidth,imageHeight} = [(x,y) | y <- [0..pred imageHeight], x <- [0..pred imageWidth]]

downTop :: Image a -> [(Int, Int)]
downTop Image{imageWidth,imageHeight} = [(x,y) | y <- reverse [0..pred imageHeight], x <- [0..pred imageWidth]]

leftRight :: Image a -> [(Int, Int)]
leftRight Image{imageWidth,imageHeight} = [(x,y) | x <- [0..pred imageWidth], y <- [0..pred imageHeight]]

rightLeft :: Image a -> [(Int, Int)]
rightLeft Image{imageWidth,imageHeight} = [(x,y) | x <- reverse [0..pred imageWidth], y <- [0..pred imageHeight]]