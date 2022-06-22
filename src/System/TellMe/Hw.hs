module System.TellMe.Hw where

import Graphics.UI.Gtk
import System.Process (spawnProcess, readProcess)
import Control.Monad.State.Lazy (void)

helloWord :: IO Widget
helloWord = do
  -- label <- labelNew $ Just "Hellow ord"
  -- let button = castToButton label
  button <- buttonNewWithLabel "caio"
  onClicked button $ void $ readProcess "xmessage" ["ciao"] ""

  return $ toWidget button
