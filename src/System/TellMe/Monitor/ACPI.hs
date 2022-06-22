module System.TellMe.Monitor.ACPI where

import Graphics.UI.Gtk (Widget)
import System.Process (readProcess)
import System.TellMe.Monitor

data Battery = B Double String


acpiB :: IO Battery
acpiB = do
  line <- readProcess "acpi" [] ""
  let parts = words line
      --pct = read $ init $ last parts :: Integer
      --pct = read $ init $ init $ parts !! 3 :: Integer
      -- time = parts !! 4
      -- TODO, need to read pow consumpion an other things... 
      time = "55:3"
  pct <- read <$> readFile "/sys/class/power_supply/BAT0/capacity"
  return $ B (fromIntegral pct / 100) time

batteryWidget :: IO Widget
batteryWidget = periodic_ 2000 acpiB --> tag "Bat: " m
  where
    m = (\(B f s) -> (f, s)) >$< mkPercent >!< mkText
