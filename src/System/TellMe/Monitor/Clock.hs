module System.TellMe.Monitor.Clock where

import Graphics.UI.Gtk (Widget)
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (formatTime)
import Data.Time.LocalTime (LocalTime, getCurrentTimeZone, utcToLocalTime)
import System.Locale (defaultTimeLocale)
import System.TellMe.Monitor

tick :: IO LocalTime
tick = do
  tz <- getCurrentTimeZone
  now <- getCurrentTime
  return $ utcToLocalTime tz now

clockWidget :: String -> IO Widget
clockWidget format = periodic_ 1000 tick --> m
  where
    m = formatTime defaultTimeLocale format >$< mkText
