module Animate.Frames.Options where

import qualified Data.Map as Map
import System.Environment (getArgs)
import Data.Map (Map)
import Data.List (intercalate)

getOptions :: IO (Maybe Options)
getOptions = do
    args <- getArgs
    return $ toOptions args

printUsage :: IO ()
printUsage = do
    putStrLn "Usage:"
    putStrLn "  animate-frames [--animation <key> <frame0.png> <frame1.png> ...] [--image <spritesheet.png>] [--metadata <target.yaml>]"
    putStrLn ""
    putStrLn "Example:"
    putStrLn $ intercalate "\n"
        [ "animate-frames"
        , "  --animation Idle idle_0000.png idle_0001.png idle_0002.png"
        , "  --animation Walk walk_0000.png walk_0001.png walk_0002.png"
        , "  --spritesheet sprite.png"
        , "  --metadata sprite.yaml"
        , ""
        ]

data Options = Options
    { optionsAnimations :: Map String [String]
    , optionsSpritesheet :: String
    , optionsMetadata :: String
    } deriving (Show, Eq)

startAnimation :: String -> Bool
startAnimation = (==) "--animation"

startSpritesheet :: String -> Bool
startSpritesheet = (==) "--spritesheet"

startMetadata :: String -> Bool
startMetadata = (==) "--metadata"

toOptions :: [String] -> Maybe Options
toOptions strArgs = do
    args <- toArgs strArgs
    let animations = toAnimations args
    spritesheet <- toSpritesheet args
    metadata <- toMetadata args
    Just Options
        { optionsAnimations = animations
        , optionsSpritesheet = spritesheet
        , optionsMetadata = metadata
        }

data Arg
    = Arg'AnimationStart
    | Arg'AnimationName String
    | Arg'AnimationFrame String
    | Arg'SpritesheetStart
    | Arg'Spritesheet String
    | Arg'MetadataStart
    | Arg'Metadata String
    deriving (Show, Eq)

data AniArg
    = AniArg'Name String
    | AniArg'Frame String
    deriving (Show, Eq)

toAnimations :: [Arg] -> Map String [String]
toAnimations args = go (toAniArgs args) Map.empty
    where
    go (b@(AniArg'Name _):bs) m = let
        (xs,ys) = span isFrame bs
        in m `Map.union` (Map.fromList [(toName b, map toName xs)]) `Map.union` go ys m
    go _ m = m

    isFrame :: AniArg -> Bool
    isFrame (AniArg'Frame _) = True
    isFrame _ = False

    toName :: AniArg -> String
    toName (AniArg'Name s) = s
    toName (AniArg'Frame s) = s

toSpritesheet :: [Arg] -> Maybe String
toSpritesheet [] = Nothing
toSpritesheet (a:as) = case a of
    Arg'Spritesheet name -> Just name
    _ -> toSpritesheet as

toMetadata :: [Arg] -> Maybe String
toMetadata [] = Nothing
toMetadata (a:as) = case a of
    Arg'Metadata name -> Just name
    _ -> toMetadata as

toAniArgs :: [Arg] -> [AniArg]
toAniArgs [] = []
toAniArgs (a:as) = case a of
    Arg'AnimationName s -> AniArg'Name s : toAniArgs as
    Arg'AnimationFrame s -> AniArg'Frame s : toAniArgs as
    _ -> toAniArgs as

toArgs :: [String] -> Maybe [Arg]
toArgs args = collapseEitherArgTokens (fmap firstPassToken args)

collapseEitherArgTokens :: [Either Arg String] -> Maybe [Arg]
collapseEitherArgTokens [] = Just []
collapseEitherArgTokens (a:as) = do
    a' <- case a of
        Left arg -> Just arg
        Right _ -> Nothing
    stepCollapse a' as

stepCollapse :: Arg -> [Either Arg String] -> Maybe [Arg]
stepCollapse _ [] = Just []
stepCollapse prev (a:as) = do
    a' <- secondPassToken prev a
    as' <- stepCollapse a' as
    return $ a' : as'

firstPassToken :: String -> Either Arg String
firstPassToken s
    | startAnimation s = Left Arg'AnimationStart
    | startSpritesheet s = Left Arg'SpritesheetStart
    | startMetadata s = Left Arg'MetadataStart
    | otherwise = Right s

secondPassToken :: Arg -> Either Arg String -> Maybe Arg
secondPassToken _ (Left arg) = Just arg
secondPassToken prev (Right arg) = case prev of
    Arg'AnimationStart -> Just $ Arg'AnimationName arg
    Arg'AnimationName _ -> Just $ Arg'AnimationFrame arg
    Arg'AnimationFrame _ -> Just $ Arg'AnimationFrame arg
    Arg'SpritesheetStart -> Just $ Arg'Spritesheet arg
    Arg'MetadataStart -> Just $ Arg'Metadata arg
    _ -> Nothing