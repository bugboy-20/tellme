module System.TellMe.Monitor where

import Control.Monad (void)
import Control.Monad.State (StateT, execStateT, liftIO)
import Data.IORef (newIORef, readIORef, writeIORef)
import qualified Graphics.Rendering.Cairo as C
import Graphics.UI.Gtk --(Widget, labelNew, labelSetText, timeoutAdd, toWidget)

type Timeout = Int

periodic :: Timeout -> IO () -> IO ()
periodic ms action = void $ timeoutAdd (action >> return True) ms

stateful :: Timeout -> s -> StateT s IO () -> IO ()
stateful ms start action = do
  ref <- newIORef start
  let callback = do
        state <- readIORef ref
        state' <- execStateT action state
        writeIORef ref state'
        return True
  void $ timeoutAdd callback ms

labelled :: String -> Widget -> IO Widget
labelled text widget = do
  box <- hBoxNew False 1
  label <- labelNew $ Just text
  boxPackStart box label PackNatural 0
  boxPackStart box widget PackNatural 0
  return $ toWidget box

monitorLabel :: Timeout -> s -> StateT s IO String -> IO Widget
monitorLabel ms start gen = do
  widget <- labelNew (Nothing :: Maybe String)
  stateful ms start $ do
    text <- gen
    liftIO $ labelSetText widget text
  return $ toWidget widget

monitorLabel' :: Timeout -> IO String -> IO Widget
monitorLabel' ms = monitorLabel ms () . liftIO

monitorBar :: Timeout -> s -> StateT s IO Double -> IO Widget
monitorBar ms start gen = do
  widget <- drawingAreaNew
  widgetSetSizeRequest widget 30 (-1)
  stateful ms start $ do
    (w, h) <- liftIO $ widgetGetSize widget
    let w' = fromIntegral w
        h' = fromIntegral h
    win <- liftIO $ widgetGetDrawWindow widget
    pct <- gen
    liftIO $ renderWithDrawable win $ do
      C.setSourceRGB 0.5 0.3 0.3
      C.rectangle 0 0 w' h'
      C.fill
      C.setSourceRGB 0 0 0
      C.rectangle 0 0 w' (pct * h')
      C.fill
  return $ toWidget widget

monitorBar' :: Timeout -> IO Double -> IO Widget
monitorBar' ms = monitorBar ms () . liftIO
