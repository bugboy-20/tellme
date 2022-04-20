module Main where

import Data.Default (def)

import System.TellMe
import System.TellMe.Monitor.ACPI
import System.TellMe.Hw
import System.TellMe.Monitor.Workspaces
-- import System.TellMe.Monitor.Clock

main :: IO ()
main = do
--  w1 <- clockWidget "%c"
-- w2 <- batteryWidget
  hw <- helloWord
  ws <- workspaces
  tellme $ def { startWidgets = [ws,hw]
               , barPosition = Top
               }
