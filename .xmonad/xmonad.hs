import qualified Data.Map as M
import XMonad
import XMonad.Actions.CycleWS
import XMonad.Actions.Search hiding (hoogle)
import XMonad.Actions.Submap
import XMonad.Actions.WindowGo
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.SetWMName
import XMonad.Prompt.Man
import XMonad.Layout.Grid
import XMonad.Layout.ResizableTile
import XMonad.Layout.Fullscreen
import XMonad.Layout.NoBorders
import XMonad.Layout.Tabbed
import XMonad.Layout.ThreeColumns
import XMonad.Layout.PerWorkspace
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig
import XMonad.Util.XSelection
import System.Directory(getHomeDirectory)
import System.IO

lbin = "/usr/local/bin"

firefox = lbin ++ "/firefox"
chrome = lbin ++ "/chrome"
thunderbird = lbin ++ "/thunderbird"
gvim = lbin ++ "/gvim"
intellij home = home ++ "/apps/idea-IU-173.3942.27/bin/idea.sh"

switch_ws ws = case filter (\(_,w) -> w == ws) $ zip (map show [0..]) myWorkspaces of
                  [(n,_)] -> wrap ("<action=xdotool set_desktop " ++ n ++ ">") "</action>" ws

main = do
   home <- getHomeDirectory
   xmproc <- spawnPipe $ lbin ++ "/xmobar-freebsd -i " ++ home ++ "/.xmobar.d/icons ~/.xmobarrc"
   xmonad $ ewmh def {focusFollowsMouse = False}
      { modMask = mod4Mask
      , manageHook = manageDocks <+> myManageHook
      , layoutHook = myLayout
      , handleEventHook = handleEventHook def <+> docksEventHook
      , logHook = dynamicLogWithPP xmobarPP
            { ppOutput = hPutStrLn xmproc
            , ppHidden = xmobarColor "#a0a0a0" "" . switch_ws
            , ppHiddenNoWindows = switch_ws
            }
      , startupHook        = setWMName "LG3D"
      , borderWidth        = 1
      , terminal           = "xterm -fg '#757575' -bg '#141414'"
      , normalBorderColor  = "#343434"
      , focusedBorderColor = "#193375" 
      , workspaces         = myWorkspaces
      } `additionalKeys` [ 
        ((mod4Mask .|. shiftMask, xK_p),   submap $ programsMap home)
      , ((mod4Mask .|. shiftMask, xK_o),   promptSelection firefox)
      , ((mod4Mask,               xK_a),   sendMessage MirrorShrink)
      , ((mod4Mask,               xK_z),   sendMessage MirrorExpand)
      , ((mod4Mask,               xK_F1),  manPrompt def)
      , ((mod4Mask,               xK_s),   submap $ searchEngineMap $ promptSearchBrowser def firefox)
      , ((mod4Mask .|. shiftMask, xK_s),   submap $ searchEngineMap $ selectSearchBrowser firefox)
      , ((mod4Mask,               xK_Tab), toggleWS)
      , ((mod4Mask,               xK_b),   sendMessage ToggleStruts)
      ]

ideLayout = fullscreenLayout ||| (avoidStruts $ Tall 1 (3/100) (1/2))
fullscreenLayout = (noBorders (fullscreenFull Full)) ||| (avoidStruts $ Full)

myLayout = onWorkspace "7:ide" ideLayout $ 
    onWorkspace "9:web" fullscreenLayout $ 
    avoidStruts (
      Tall 1 (3/100) (1/2) |||
      Mirror (Tall 1 (3/100) (1/2)) |||
      Grid |||
      ThreeColMid 1 (3/100) (1/3) |||
      tabbed shrinkText tabConfig |||
      noBorders (fullscreenFull Full))

tabConfig = def {
    activeBorderColor = "#193375",
    activeTextColor = "#a0a0a0",
    activeColor = "#282828",
    inactiveBorderColor = "#343434",
    inactiveTextColor = "#757575",
    inactiveColor = "#141414",
    fontName = "-*-Fixed-Bold-R-Normal-*-15-*-*-*-*-*-*-*222"
}

myWorkspaces = ["1", "2", "3", "4", "5", "6:dev", "7:ide", "8:mail", "9:web"]

programsMap home = M.fromList $
      [ ((0, xK_c), spawn chrome)
      , ((0, xK_f), spawn firefox)
      , ((0, xK_t), spawn thunderbird)
      , ((0, xK_v), spawn gvim)
      , ((0, xK_i), spawn $ intellij home)
      ]

dictCc = searchEngine "dictcc" "http://dict.cc/?s="
hoogleSearch = searchEngine "hoogle" "https://hoogle.haskell.org/?hoogle="
duckDuckGo = searchEngine "duckduckgo" "http://duckduckgo.com/?q="
hoogle = searchEngine "hoogle" "https://hoogle.haskell.org/?hoogle="

searchEngineMap method = M.fromList $
      [ ((0, xK_e), method dictCc)
      , ((0, xK_d), method duckDuckGo)
      , ((0, xK_g), method google)
      , ((0, xK_i), method imdb)
      , ((0, xK_h), method hoogleSearch)
      , ((0, xK_w), method wikipedia)
      ]

myManageHook :: ManageHook
myManageHook = composeAll [ 
      className =? "jetbrains-idea"             --> doShift "7:ide"
    , className =? "Thunderbird"                --> doShift "8:mail"
    , className =? "Firefox"                    --> doShift "9:web"
    , className =? "Opera"                      --> doShift "9:web"
    , className =? "Chromium-browser"           --> doShift "9:web"
    ]
