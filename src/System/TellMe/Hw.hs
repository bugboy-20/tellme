module System.TellMe.Hw where

import Graphics.UI.Gtk

helloWord :: IO Widget
helloWord = do
  label <- labelNew (Just "ciao")
  return $ toWidget label
