module System.TellMe.Monitor.Workspaces where


import System.TellMe.Monitor

import Control.Monad
import System.Process

import Graphics.UI.Gtk (Widget)
import Graphics.UI.Gtk.Display.Label (labelNew)
import Graphics.UI.Gtk.Abstract.Widget (toWidget)


workspaces :: IO Widget
workspaces = periodic_ 10 xmonadWSPipe --> m
  where
    m = mkText

readWs = do
  ws <- xmonadWSPipe
  label <- labelNew $ Just ws
  return $ toWidget label


xmonadWSPipe :: IO String
xmonadWSPipe = readProcess "cat" ["/tmp/.xmonad-workspace-log"] ""
