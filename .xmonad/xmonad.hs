import qualified Data.Map as M
import XMonad
import XMonad.Actions.CycleWS
import XMonad.Actions.Search hiding (hoogle)
import XMonad.Actions.Submap
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.SetWMName
import XMonad.Layout.Fullscreen
import XMonad.Layout.Grid
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace
import XMonad.Layout.ResizableTile
import XMonad.Layout.Tabbed
import XMonad.Layout.ThreeColumns
import XMonad.Prompt.Man
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig
import XMonad.Util.XSelection
import System.IO

firefox = "/usr/local/bin/firefox"
chrome = "/usr/local/bin/chrome"
thunderbird = "/usr/local/bin/thunderbird"
gvim = "/usr/local/bin/gvim"
dmenuRun = "dmenu_run -nb '#141414' -nf '#757575' -fn '-*-Fixed-*-R-Normal-*-13-*-*-*-*-*-*-*'"

switch_ws ws = case filter (\(_,w) -> w == ws) $ zip (map show [0..]) myWorkspaces of
                  [(n,_)] -> wrap ("<action=xdotool set_desktop " ++ n ++ ">") "</action>" ws

main = do
   xmproc <- spawnPipe "/usr/bin/xmobar ~/.xmobarrc"
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
        ((mod4Mask,               xK_p),   spawn dmenuRun)
      , ((mod4Mask .|. shiftMask, xK_p),   submap programsMap)
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

myWorkspaces = ["1", "2", "3", "4", "5", "6:dev", "7:eclipse", "8:mail", "9:web"]

programsMap = M.fromList $
      [ ((0, xK_c), spawn chrome)
      , ((0, xK_f), spawn firefox)
      , ((0, xK_t), spawn thunderbird)
      , ((0, xK_v), spawn gvim)
      ]

dictCc = searchEngine "dictcc" "http://dict.cc/?s="
duckDuckGo = searchEngine "duckduckgo" "http://duckduckgo.com/?q="
hoogle = searchEngine "hoogle" "https://hoogle.haskell.org/?hoogle="

searchEngineMap method = M.fromList $
      [ ((0, xK_e), method dictCc)
      , ((0, xK_d), method duckDuckGo)
      , ((0, xK_g), method google)
      , ((0, xK_i), method imdb)
      , ((0, xK_h), method hoogle)
      , ((0, xK_w), method wikipedia)
      ]

myManageHook :: ManageHook
myManageHook = composeAll [ 
      className =? "Eclipse"                    --> doShift "7:ide"
    , className =? "jetbrains-idea"             --> doShift "7:ide"
    , className =? "Thunderbird"                --> doShift "8:mail"
    , className =? "Firefox"                    --> doShift "9:web"
    , className =? "Opera"                      --> doShift "9:web"
    , className =? "Chromium-browser"           --> doShift "9:web"
    ]
