{-# LANGUAGE TupleSections #-}

module System.TellMe.Monitor where

import Control.Monad (void)
import Control.Monad.State (StateT, runStateT, liftIO)
import Data.IORef (newIORef, readIORef, writeIORef)
import qualified Graphics.Rendering.Cairo as C
import Graphics.UI.Gtk
import Text.Printf (printf)

class ContraFunctor m where
  (>$<) :: (a -> b) -> m b -> m a

class ContraApplicative m where
  empty :: m a
  (>*<) :: m a -> m b -> m (Either a b)

class Concurrent m where
  dup :: m (Either a b) -> m (a, b)

infixr 8 >$<
infixr 8 >*<

(>!<) :: (ContraApplicative m, Concurrent m) => m a -> m b -> m (a, b)
m1 >!< m2 = dup $ m1 >*< m2

infixr 8 >!<

type Timeout = Int

type Sink a = a -> IO ()
type Source a = Sink a -> IO ()

callback :: s -> StateT s IO a -> Sink a -> IO (IO Bool)
callback start gen sink = do
  ref <- newIORef start
  let cb = do
        state <- readIORef ref
        (v, state') <- runStateT gen state
        writeIORef ref state'
        sink v
        return True
  return cb

periodic :: Timeout -> s -> StateT s IO a -> Source a
periodic ms start gen sink = do
  cb <- callback start gen sink
  void $ timeoutAddFull cb priorityLow ms

periodic_ :: Timeout -> IO a -> Source a
periodic_ ms gen = periodic ms () (liftIO gen)

newtype Monitor a = M { runMonitor :: IO (Widget, Sink a) }

(-->) :: Source a -> Monitor a -> IO Widget
(-->) source mon = do
  (w, sink) <- runMonitor mon
  source sink
  return w

instance ContraFunctor Monitor where
  f >$< mon = M $ do
    (w, fill) <- runMonitor mon
    return (w, fill . f)

boxed :: Widget -> Widget -> IO Widget
boxed w1 w2 = do
  box <- hBoxNew False 1
  boxPackStart box w1 PackNatural 0
  boxPackStart box w2 PackNatural 0
  return $ toWidget box

instance ContraApplicative Monitor where
  empty = M $ do
    box <- hBoxNew False 0
    return (toWidget box, return . const ())
  mon1 >*< mon2 = M $ do
    (w1, sink1) <- runMonitor mon1
    (w2, sink2) <- runMonitor mon2
    w <- boxed w1 w2
    let sink (Left v1) = sink1 v1
        sink (Right v2) = sink2 v2
    return (w, sink)

instance Concurrent Monitor where
  dup mon = M $ do
    (w, sink) <- runMonitor mon
    let sink' (a, b) = sink (Left a) >> sink (Right b)
    return (w, sink')

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
mkPercent = split . clamp >$< mkBar 10 >!< mkText
  where
    split x = (x, printf "%3.0f%%" $ 100 * x)
    clamp x | x < 0 = 0
            | x > 1 = 1
            | otherwise = x

tag :: String -> Monitor a -> Monitor a
tag s mon = M $ do
  (w, sink) <- runMonitor $ mkText >*< mon
  sink (Left s)
  return (w, sink . Right)


-- triggered :: FD -> s -> StateT s IO a -> Monitor a -> IO Widget
-- triggered fd start gen (M m) = do
--   (widget, fill) <- m
--   cb <- callback start gen fill
--   void $ inputAdd fd [IOIn] priorityLow cb
--   return widget

-- triggered_ :: FD -> IO a -> Monitor a -> IO Widget
-- triggered_ fd gen = triggered fd () (liftIO gen)

-- parsed :: FD -> Parser a -> Monitor a -> IO Widget
-- parsed fd = triggered fd Nothing act
--   where
--     act = do
--       buff <- liftIO $ readFromFd fd
--       loop buff
--     loop buff | null buff =

--       s <- get
--       case s of
--         Nothing -> undefined
--         Just result -> do
