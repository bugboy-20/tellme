module System.TellMe.Monitor.PulseAudio where


import Graphics.UI.Gtk (Widget)
import System.Process (readProcess)
import System.TellMe.Monitor

type RampVolume = String -- text to be shown on volume ramp

newtype Audio = A RampVolume


pulseAdioWidget :: IO Widget
pulseAdioWidget = periodic_ 2000 defaultSink --> tag "Volume: " m
  where
    m = (\(A v) -> v) >$< mkText

defaultSink :: IO Audio
defaultSink = do
  pactl <- readProcess "pactl" ["get-sink-volume","@DEFAULT_SINK@"] ""
  let vol = head $ filter (\w -> last w == '%') $ words pactl
  return (A vol)
