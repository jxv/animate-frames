module Animate.Frames (main) where

import System.Environment (getArgs)

{-
animate-frames frames_0.png frames_1.png ... sprite.png
-}

main :: IO ()
main = do
  args <- getArgs
  case args of
    args'@(_:_) -> putStrLn $ show args
    _ -> printUsage

printUsage :: IO ()
printUsage = putStrLn "animate-frames frame_0.png frame_1.png ... spritesheet.png"