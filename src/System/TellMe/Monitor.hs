module System.TellMe.Monitor where

import Control.Monad (void)
import Control.Monad.State (StateT, execStateT)
import Data.IORef (newIORef, readIORef, writeIORef)
import Graphics.UI.Gtk (Widget, timeoutAdd)

type Timeout = Int

periodic :: Timeout -> IO () -> IO ()
periodic ms action = void $ timeoutAdd (action >> return True) ms

stateful :: Timeout -> StateT s IO () -> s -> IO ()
stateful ms action start = do
  ref <- newIORef start
  let callback = do
        state <- readIORef ref
        state' <- execStateT action state
        writeIORef ref state'
        return True
  void $ timeoutAdd callback ms
