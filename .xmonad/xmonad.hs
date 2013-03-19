import qualified Data.Map as M
import XMonad
import XMonad.Actions.CycleWS
import XMonad.Actions.Search
import XMonad.Actions.Submap
import XMonad.Actions.WindowGo
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.DynamicLog
import XMonad.Layout.Grid
import XMonad.Layout.ResizableTile
import XMonad.Prompt
import XMonad.Prompt.Man
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig
import XMonad.Util.XSelection
import System.IO

firefox = "/usr/local/bin/firefox"
thunderbird = "/usr/local/bin/thunderbird"
eclipse = "GTK2_RC_FILES=/usr/share/themes/Clearlooks/gtk-2.0/gtkrc:~/.gtkrc-eclipse eclipse"
gvim = "/usr/local/bin/gvim"

main = do
   xmproc <- spawnPipe "/usr/bin/xmobar ~/.xmobarrc"
   xmonad $ ewmh defaultConfig
      { modMask = mod4Mask
      , manageHook = manageDocks <+> myManageHook
      , layoutHook = avoidStruts $ layoutHook defaultConfig ||| Grid ||| ResizableTall 1 (3/100) (1/2) []
      , logHook = dynamicLogWithPP xmobarPP
            { ppOutput = hPutStrLn xmproc
            , ppHidden = (\ws -> 
                  case filter (\(_,w) -> w == ws) $ zip (map show [0..]) myWorkspaces of
                     [(n,_)] -> wrap ("<action=ws:" ++ n ++ ">") "</action>" ws
                     _       -> ws)
            }
      , borderWidth        = 1
      , terminal           = "xterm -fg '#757575' -bg '#141414'"
      , normalBorderColor  = "#343434"
      , focusedBorderColor = "#193375" 
      , workspaces         = myWorkspaces
      } `additionalKeys` [ 
        ((mod4Mask .|. shiftMask, xK_p),   submap programsMap)
      , ((mod4Mask .|. shiftMask, xK_o),   promptSelection firefox)
      , ((mod4Mask,               xK_a),   sendMessage MirrorShrink)
      , ((mod4Mask,               xK_z),   sendMessage MirrorExpand)
      , ((mod4Mask,               xK_F1),  manPrompt defaultXPConfig)
      , ((mod4Mask,               xK_s),   submap $ searchEngineMap $ promptSearchBrowser defaultXPConfig firefox)
      , ((mod4Mask .|. shiftMask, xK_s),   submap $ searchEngineMap $ selectSearchBrowser firefox)
      , ((mod4Mask,               xK_Tab), toggleWS)
      ]

myWorkspaces = ["1", "2", "3", "4", "5", "6:dev", "7:eclipse", "8:mail", "9:web"]

programsMap = M.fromList $
      [ ((0, xK_e), spawn eclipse)
      , ((0, xK_t), spawn thunderbird)
      , ((0, xK_f), spawn firefox)
      , ((0, xK_v), spawn gvim)
      ]

dictCc = searchEngine "dictcc" "http://dict.cc/?s="

searchEngineMap method = M.fromList $
      [ ((0, xK_d), method dictCc)
      , ((0, xK_g), method google)
      , ((0, xK_h), method hoogle)
      , ((0, xK_w), method wikipedia)
      ]

myManageHook :: ManageHook
myManageHook = composeAll [ 
      className =? "Eclipse"                    --> doShift "7:eclipse"
    , className =? "Thunderbird"                --> doShift "8:mail"
    , className =? "Firefox"                    --> doShift "9:web"
    , className =? "Opera"                      --> doShift "9:web"
    ]
