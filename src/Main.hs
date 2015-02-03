module Main where

import Data.Default (def)

import System.TellMe
import System.TellMe.Monitor.ACPI
import System.TellMe.Monitor.Clock

main :: IO ()
main = do
  w1 <- clockWidget "%c"
  w2 <- batteryWidget
  tellme $ def { startWidgets = [w1, w2] }
