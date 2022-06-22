module Main where

import Data.Default (def)

import System.TellMe
import System.TellMe.Monitor.ACPI
import System.TellMe.Hw
import System.TellMe.Monitor.Workspaces
import System.TellMe.Monitor.PulseAudio
import System.TellMe.Monitor.Clock
import System.TellMe.Monitor.Custom

-- import System.TellMe.Monitor.Clock

main :: IO ()
main = do
  w1 <- clockWidget "%c"
  w2 <- batteryWidget
  hw <- helloWord
  v <- pulseAdioWidget 
  c <- custom "echo" ["ciao"] 100
  --ws <- workspaces
  tellme $ def { startWidgets = [v,hw]
               , endWidgets = [c,w2,w1]
               , barPosition = Top
               }
