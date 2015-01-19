module System.TellMe.Monitor where

import Graphics.UI.Gtk (Widget)

import System.TellMe.Schedule

data Monitor = Monitor { widget :: Widget
                       , action :: Action
                       }
