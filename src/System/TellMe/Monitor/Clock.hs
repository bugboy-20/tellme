module System.TellMe.Monitor.Clock where

import Graphics.UI.Gtk (Widget)
import Data.Time.Clock (UTCTime, getCurrentTime)
import Data.Time.Format (formatTime)
import Data.Time.LocalTime (getCurrentTimeZone, utcToLocalTime)
import System.Locale (defaultTimeLocale)
import System.TellMe.Monitor

clock :: String -> IO Widget
clock format = do
  tz <- getCurrentTimeZone
  monitorLabel' 1000 $ do
    now <- getCurrentTime
    let local = utcToLocalTime tz now
    return $ formatTime defaultTimeLocale format local
