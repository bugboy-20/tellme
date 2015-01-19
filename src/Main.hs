module Main where

import Data.Default (Default(..))
import Graphics.UI.Gtk

import System.TellMe.Monitor
import System.TellMe.Schedule

data Position = Top | Bottom
  deriving (Show, Eq)

data Config = Config { screenNumber :: Int
                     , monitorNumber :: Int
                     , barHeight :: Int
                     , barPosition :: Position
                     , widgetSpacing :: Int
                     , startMonitors :: [Monitor]
                     , endMonitors :: [Monitor]
                     }

instance Default Config where
  def = Config { screenNumber = 0
               , monitorNumber = 0
               , barHeight = 25
               , barPosition = Bottom
               , widgetSpacing = 10
               , startMonitors = []
               , endMonitors = []
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

-- installMonitors cfg box = do
--   mapM_ (add boxPackStart) $ startMonitors cfg
--   mapM_ (add boxPackEnd) $ endMonitors cfg
--   queue <- newIORef [(0, action) | Monitor action _ <- startMonitors cfg ++ endMonitors cfg]
--   let loop = do
--        q <- readIORef queue

       
           
--   bleh <- mapM (\a -> a >>= \i -> (i, a)) actions
--     where
--       h = barHeight cfg
--       add method (Monitor action widget) = do
--         widgetSetSizeRequest widget (-1) h
--         method box widget PackNatural 0


tellme :: Config -> IO ()
tellme cfg = do
  initGUI
  Just disp <- displayGetDefault
  screen <- displayGetScreen disp $ screenNumber cfg
  window <- windowNew
  widgetSetName window "TellMe"
  windowSetTypeHint window WindowTypeHintDock
  windowSetScreen window screen
  setSize cfg window
  on screen screenMonitorsChanged $ setSize cfg window
  box <- hBoxNew False $ widgetSpacing cfg
  containerAdd window box
  -- (loop, nextTime) <- installMonitors cfg box
  -- timeoutAdd loop nextTime
  widgetShowAll window
  mainGUI

main = tellme def
