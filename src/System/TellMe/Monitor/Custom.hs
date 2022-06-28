module System.TellMe.Monitor.Custom where

import System.TellMe.Monitor
import System.Process (readProcess)
import Graphics.UI.Gtk (Widget)

type Command = String
type Args = [String]

command :: Command -> Args -> IO Command
command c a = readProcess c a ""

custom :: Command -> Args -> Int -> IO Widget
custom cmd a t = periodic_ t (command cmd a) --> mkText
