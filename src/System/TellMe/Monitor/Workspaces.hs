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

  {--
    add in the main
    > safeSpawn "mkfifo" ["/tmp/.xmonad-title-log"]

    and something like this in xmonad config
    > winset <- gets windowset
    > let currWs = W.currentTag winset
    > let wss = map W.tag $ W.workspaces winset
    > let usedWs = map W.tag . filter (Data.Maybe.isJust . W.stack) $ W.workspaces winset
    > let wsStr = join $ map (\w -> cc w usedWs currWs) $ sort' wss

    > io $ appendFile "/tmp/.xmonad-workspace-log" (wsStr ++ "\n")

    > where sort' = sortBy (compare `on` (!! 0))
    >       cc ws usedWs currWs
    >         | ws == currWs = " \61754  " --  -- "[" ++ ws ++ "]"
    >         | ws `elem` usedWs = " \61842  " --  -- "<" ++ ws ++ ">"
    >         | otherwise    = " \61713  " --  -- " " ++ ws ++ " "
    
--}
