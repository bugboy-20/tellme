module System.TellMe.Monitor.Clock where

import Graphics.UI.Gtk (Widget, labelNew, labelSetText, toWidget)
import Data.Time.Clock (UTCTime, getCurrentTime)
import Data.Time.Format (formatTime)
import Data.Time.LocalTime (getCurrentTimeZone, utcToLocalTime)
import System.Locale (defaultTimeLocale)
import System.TellMe.Monitor

clock :: String -> IO Widget
clock format = do
  widget <- labelNew (Nothing :: Maybe String)
  tz <- getCurrentTimeZone
  periodic 1000 $ do
    now <- getCurrentTime
    let local = utcToLocalTime tz now
        s = formatTime defaultTimeLocale format local
    labelSetText widget s
  return $ toWidget widget
