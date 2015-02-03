module System.TellMe where

import Control.Monad (void)
import Data.Default (Default(..))
import Graphics.UI.Gtk

data Position = Top | Bottom
  deriving (Show, Eq)

data Config = Config { screenNumber :: Int
                     , monitorNumber :: Int
                     , barHeight :: Int
                     , barPosition :: Position
                     , widgetSpacing :: Int
                     , startWidgets :: [Widget]
                     , endWidgets :: [Widget]
                     }

instance Default Config where
  def = Config { screenNumber = 0
               , monitorNumber = 0
               , barHeight = 25
               , barPosition = Bottom
               , widgetSpacing = 10
               , startWidgets = []
               , endWidgets = []
               }

setSize :: Config -> Window -> IO ()
setSize cfg window = do
  screen <- windowGetScreen window
  Rectangle x y w h <- screenGetMonitorGeometry screen $ monitorNumber cfg
  let yoff = case barPosition cfg of
        Top -> 0
        Bottom -> h - barHeight cfg
  windowMove window x $ y + yoff
  windowSetGeometryHints window
                         (Nothing :: Maybe Widget)
                         (Just (w, barHeight cfg))
                         (Just (w, barHeight cfg))
                         Nothing
                         Nothing
                         Nothing

tellme :: Config -> IO ()
tellme cfg = do
  void initGUI
  Just disp <- displayGetDefault
  screen <- displayGetScreen disp $ screenNumber cfg
  window <- windowNew
  widgetSetName window "TellMe"
  windowSetTypeHint window WindowTypeHintDock
  windowSetScreen window screen
  setSize cfg window
  void $ on screen screenMonitorsChanged $ setSize cfg window
  box <- hBoxNew False $ widgetSpacing cfg
  containerAdd window box
  let addWidgets method widget = do
        widgetSetSizeRequest widget (-1) $ barHeight cfg
        method box widget PackNatural 0
  mapM_ (addWidgets boxPackStart) $ startWidgets cfg
  mapM_ (addWidgets boxPackEnd) $ endWidgets cfg
  widgetShowAll window
  mainGUI
