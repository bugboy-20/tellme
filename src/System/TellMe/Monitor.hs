{-# LANGUAGE TupleSections #-}

module System.TellMe.Monitor where

import Control.Monad (void)
import Control.Monad.State (StateT, runStateT, liftIO)
import Data.IORef (newIORef, readIORef, writeIORef)
import qualified Graphics.Rendering.Cairo as C
import Graphics.UI.Gtk
import Text.Printf (printf)

type Timeout = Int

data Monitor a = M (IO (Widget, a -> IO ()))

class ContraFunctor m where
  (>$<) :: (a -> b) -> m b -> m a

infixr 8 >$<

class ContraApplicative m where
  empty :: m a
  (>*<) :: m a -> m b -> m (a, b)

infixr 8 >*<

instance ContraFunctor Monitor where
  f >$< (M m) = M $ do
    (w, fill) <- m
    return (w, fill . f)

instance ContraApplicative Monitor where
  empty = M $ do
    box <- hBoxNew False 0
    return (toWidget box, return . const ())
  (M m1) >*< (M m2) = M $ do
    (w1, fill1) <- m1
    (w2, fill2) <- m2
    box <- hBoxNew False 1
    boxPackStart box w1 PackNatural 0
    boxPackStart box w2 PackNatural 0
    let fill (v1, v2) = fill1 v1 >> fill2 v2
    return (toWidget box, fill)

mkText :: Monitor String
mkText = M $ do
  label <- labelNew (Nothing :: Maybe String)
  return (toWidget label, labelSetText label)

mkBar :: Int -> Monitor Double
mkBar width = M $ do
  drawArea <- drawingAreaNew
  widgetSetSizeRequest drawArea width (-1)
  let fill v = do
        (w, h) <- widgetGetSize drawArea
        let w' = fromIntegral w
            h' = fromIntegral h
        win <- widgetGetDrawWindow drawArea
        renderWithDrawable win $ do
          C.setSourceRGB 0.5 0.3 0.3
          C.rectangle 0 0 w' h'
          C.fill
          C.setSourceRGB 0 0 0
          C.rectangle 0 (v * h') w' h'
          C.fill
  return (toWidget drawArea, fill)

mkPercent :: Monitor Double
mkPercent = split . clamp >$< mkBar 10 >*< mkText
  where
    split x = (x, printf "%3.0f%%" $ 100 * x)
    clamp x | x < 0 = 0
            | x > 1 = 1
            | otherwise = x

labelled :: String -> Monitor a -> Monitor a
labelled s m = (s, ) >$< mkText >*< m

periodic :: Timeout -> s -> StateT s IO a -> Monitor a -> IO Widget
periodic ms start gen (M m) = do
  (widget, fill) <- m
  ref <- newIORef start
  let callback = do
        state <- readIORef ref
        (v, state') <- runStateT gen state
        writeIORef ref state'
        fill v
        return True
  void $ timeoutAdd callback ms
  return widget

periodic_ :: Timeout -> IO a -> Monitor a -> IO Widget
periodic_ ms gen = periodic ms () (liftIO gen)

--triggered :: FD -> s -> StateT s IO a -> Monitor a -> IO Widget
