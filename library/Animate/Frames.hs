module Animate.Frames (main) where

import Animate.Frames.Options (getOptions, printUsage, Options(..))

main :: IO ()
main = do
  options' <- getOptions
  case options' of
    Nothing -> printUsage
    Just options -> print options
