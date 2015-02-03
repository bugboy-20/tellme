module Main where

import Data.Default (def)

import System.TellMe
import System.TellMe.Monitor.Clock

main :: IO ()
main = do
  c1 <- clockWidget "%c"
  c2 <- clockWidget "%c"
  tellme $ def { startWidgets = [c1, c2] }
