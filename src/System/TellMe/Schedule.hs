module System.TellMe.Schedule where

import Control.Monad (void)
import Control.Monad.State (StateT, execStateT)
import Data.Function (on)
import Data.List (sortBy)
import Data.Time.Clock (UTCTime, addUTCTime, diffUTCTime, getCurrentTime)
import Data.Time.Format ()
import Graphics.UI.Gtk (timeoutAdd)

data Action = Action { runAction :: UTCTime -> IO Future }

data Future = Future
              { futureWhen :: UTCTime
              , futureAction :: Action
              }

instance Show Future where
  show f = show (futureWhen f) ++ ": ?"

type Schedule = [Future]

current :: Schedule -> UTCTime -> ([Action], Schedule)
current futures now = (map futureAction before, after)
  where
    (before, after) = span ((<= now) . futureWhen) futures

clock :: Schedule -> UTCTime
clock = futureWhen . head

merge :: Schedule -> Schedule -> Schedule
merge s1 s2 = sortBy (compare `on` futureWhen) $ s1 ++ s2

schedule :: [Action] -> IO ()
schedule actions = getCurrentTime >>= void . execute actions []
  where
    execute todo after now = do
      done <- mapM (flip runAction now) todo
      let futures = merge after done
      run futures now
    callback futures = getCurrentTime >>= run futures
    run futures now =
      let (todo, after) = current futures now
      in case todo of
        [] -> reschedule after now
        _ -> execute todo after now
    reschedule futures now =
      let when = clock futures
          d = diffUTCTime when now
          ms = ceiling $ d * 1000
      in timeoutAdd (callback futures) ms >> return False

ms2delta :: Int -> UTCTime -> UTCTime
ms2delta ms = addUTCTime ms'
  where
    ms' = realToFrac $ 1000 * (fromIntegral ms :: Double)

simpleAction :: Int -> IO () -> Action
simpleAction ms io = Action go
  where
    delta = ms2delta ms
    go now = do
      io
      return $ Future (delta now) (Action go)

statefulAction :: Int -> StateT s IO () -> s -> Action
statefulAction ms f initial = Action $ go initial
  where
    delta = ms2delta ms
    go state now = do
      state' <- execStateT f state
      return $ Future (delta now) (Action $ go state')
