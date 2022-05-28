module Main where

import Data.Default (def)

import System.TellMe
import System.TellMe.Monitor.ACPI
import System.TellMe.Hw
import System.TellMe.Monitor.Workspaces
import System.TellMe.Monitor.PulseAudio
import System.TellMe.Monitor.Clock

-- import System.TellMe.Monitor.Clock

main :: IO ()
main = do
  w1 <- clockWidget "%c"
  w2 <- batteryWidget
  hw <- helloWord
  v <- pulseAdioWidget 
  --ws <- workspaces
  tellme $ def { startWidgets = [v]
               , endWidgets = [w2,w1]
               , barPosition = Top
               }
