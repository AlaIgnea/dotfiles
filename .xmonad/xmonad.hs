{-- xmonad.hs 
  Required:
    - xmonad-darcs as of 01-07-2010
    - xmonad-contrib-darcs as of 01-07-2010
    - RssReader.hs and Dzen.hs (http://pbrisbin.com/xmonad/docs/)
    - haskell-http
    - haskell-tagsoup
    - dzen2
    - conky
    - profont
--}
 
-- Import stuff
import XMonad
import qualified XMonad.StackSet as W
import qualified Data.Map as M
import XMonad.Util.EZConfig(additionalKeys)
import System.Exit
import Graphics.X11.Xlib
import System.IO

-- actions
import XMonad.Actions.CycleWS
import XMonad.Actions.WindowGo
import qualified XMonad.Actions.Search as S
import XMonad.Actions.Search
import qualified XMonad.Actions.Submap as SM
import XMonad.Actions.GridSelect

-- utils
import XMonad.Util.Scratchpad (scratchpadSpawnAction, scratchpadManageHook, scratchpadFilterOutWorkspace)
import XMonad.Util.Run(spawnPipe)
import qualified XMonad.Prompt as P
import XMonad.Prompt.Shell
import XMonad.Prompt
 
-- hooks
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.ManageHelpers

-- layouts
import XMonad.Layout.NoBorders
import XMonad.Layout.ResizableTile
import XMonad.Layout.Reflect
import XMonad.Layout.IM
import XMonad.Layout.Tabbed
import XMonad.Layout.PerWorkspace (onWorkspace)
import XMonad.Layout.Grid

-- Data.Ratio for IM Layout
import Data.Ratio ((%))

-- Modules in ~/.xmonad/lib
import Dzen
import RssReader

-- Main --
main = do
  d <- spawnDzen myLeftBar
  spawnDzen dzenConf >>= spawnReader readerConf   
  spawn $ "conky -c ~/.dzen_conkyrc | " ++ show myRightBar
  xmonad $ withUrgencyHook NoUrgencyHook $ defaultConfig
    { manageHook            = myManageHook
    , layoutHook            = myLayoutHook 
    , borderWidth           = myBorderWidth
    , normalBorderColor     = myNormalBorderColor
    , focusedBorderColor    = myFocusedBorderColor
    , keys                  = myKeys
    , logHook               = myLogHook d
    , modMask               = myModMask
    , terminal              = myTerminal
    , workspaces            = myWorkspaces
    , focusFollowsMouse     = False
    }
 
-- hooks
-- automaticly switching app to workspace
myManageHook :: ManageHook
myManageHook = scratchpadManageHook (W.RationalRect 0.25 0.375 0.5 0.35) <+> ( composeAll . concat $
  [ [ isFullscreen                       --> myDoFullFloat                              ]
  , [ className =? c                     --> doCenterFloat     |  c    <- myCenterFloats ]
  , [ className =? c                     --> doShift "1:chat"  |  c    <- myChats        ]
  , [ className =? c                     --> doShift "2:web"   |  c    <- myWebs         ]
  , [ className =? c                     --> doShift "4:doc"   |  c    <- myDocs         ]
  , [ className =? c                     --> doShift "6:vbox"  |  c    <- myVirt         ]
  , [ className =? c                     --> doShift "7:music" |  c    <- myQL           ]
  , [ className =? c                     --> doShift "8:vid"   |  c    <- myVid          ]
  , [ className =? c                     --> doShift "9:gimp"  |  c    <- myGimp         ]
  , [ title     =? c                     --> doShift "8:vid"   |  c    <- myVids         ]
  ] ) <+> manageDocks
  
  where

    role           = stringProperty "WM_WINDOW_ROLE"
    name           = stringProperty "WM_NAME"

    -- classnames
    myCenterFloats = ["Xmessage","Zenity","feh","GQview"]   ++
                     ["XFontSel","Save As...","MPlayer"]

    myChats        = ["Pino","Xchat","Choqok"]

    myWebs         = ["Namoroka","Firefox","Google-chrome"] ++
                     ["Chromium", "Opera"]

    myDocs         = ["OpenOffice.org 3.2","Evince","Epdfview"] ++
                     ["LibreOffice"]

    myGimp         = ["Gimp"]

    myQL           = ["Quodlibet"]

    myVid          = ["MPlayer"]

    myVirt         = ["VirtualBox","Wine"]

    myVids         = ["VLC media player"]

-- a trick for fullscreen but stil allow focusing of other WSs
myDoFullFloat :: ManageHook
myDoFullFloat = doF W.focusDown <+> doFullFloat

--logHook
myLogHook :: Handle -> X ()
myLogHook h = dynamicLogWithPP $ customPP { ppOutput = hPutStrLn h }


---- Statusbar

myLeftBar :: DzenConf
myLeftBar = defaultDzen
  { x_position  = 0
  , width       = 825
  }

myRightBar :: DzenConf
myRightBar = myLeftBar
  { x_position  = 1125
  , width       = 155
  , alignment   = RightAlign
  }

--dzenConf
dzenConf :: DzenConf
dzenConf = defaultDzen
  { x_position  = 825
  , width       = 300
  , fg_color    = "#606060"
  , bg_color    = "#303030"
  }

--readerConf
readerConf :: ReaderConf
readerConf = defaultReaderConf
  { limit       = 10
  , titleFormat = dzenFG "#606060"
  , descrFormat = shorten 400 
  , tickerWidth = 300 
  }

  where
    -- some helpers
    dzenFG c s  = concat ["^fg(", c, ")", s, "^fg()"]
    shorten n s = if length s > n then (take n s) ++ "..." else s

---- Looks --
---- bar
customPP :: PP
customPP = defaultPP
  { ppHidden  = dzenColor "#909090" ""
  , ppCurrent = dzenColor "#303030" "#909090" . wrap "[" "]"
  , ppUrgent  = dzenColor "#FF0000" "" . wrap "*" "*"
  , ppLayout  = dzenColor "#909090" ""
  , ppTitle   = shorten 80
  , ppSep     = "  "
  }

-- some nice colors for the prompt windows to match the xmobar status bar.
myXPConfig = defaultXPConfig
  { XMonad.Prompt.font = "-*-profont-*-*-*-*-11-*-*-*-*-*-*-*"
  , fgColor            = "#909090"
  , bgColor            = "#303030"
  , bgHLight           = "#000000"
  , fgHLight           = "#FF0000"
  , position           = Top
  }
 
--- My Theme For Tabbed layout
myTheme = defaultTheme 
  { decoHeight          = 16
  , activeColor         = "#A6C292"
  , activeBorderColor   = "#A6C292"
  , activeTextColor     = "#000000"
  , inactiveBorderColor = "#000000"
  }

--LayoutHook
myLayoutHook = onWorkspace "1:chat" webL $ onWorkspace "2:web" webL $ onWorkspace "6:vbox" fullL $ onWorkspace "8:vid" fullL $ onWorkspace "9:gimp" gimpL $ standardLayouts
  where
    standardLayouts = avoidStruts  $ (tiled ||| reflectTiled ||| Mirror tiled ||| Full)
 
    --Layouts
    tiled = smartBorders (ResizableTall 1 (2/100) (1/2) [])
    reflectTiled = (reflectHoriz tiled)
    tabLayout = (tabbed shrinkText myTheme)
    full = noBorders Full
 
    --Gimp Layout
    gimpL = avoidStruts $ smartBorders $ withIM (0.11) (Role "gimp-toolbox") $ reflectHoriz $ withIM (0.15) (Role "gimp-dock") Full
 
    --Web Layout
    webL = avoidStruts $ tabLayout ||| tiled ||| reflectHoriz tiled ||| full

    --VirtualLayout
    fullL = avoidStruts $ full

-------------------------------------------------------------------------------
---- Terminal --
myTerminal :: String
myTerminal = "urxvt"

-------------------------------------------------------------------------------
-- Keys/Button bindings --
-- modmask
myModMask :: KeyMask
myModMask = mod4Mask
altMask   = mod1Mask

-- borders
myBorderWidth :: Dimension
myBorderWidth = 1
-- 
myNormalBorderColor, myFocusedBorderColor :: String
myNormalBorderColor  = "#333333"
myFocusedBorderColor = "#FF0000"
--
  
--Workspaces
myWorkspaces :: [WorkspaceId]
myWorkspaces = ["1:chat", "2:web", "3:code", "4:doc", "5:foo", "6:vbox", "7:music", "8:vid", "9:gimp"]

-- Switch to the "web" workspace
viewWeb = windows (W.greedyView "2:web") -- (0,0a)
--

-- keys
myKeys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
myKeys conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $

  -- kiling programs
  [ ((modMask ,                xK_Return ), spawn $ XMonad.terminal conf       )
  , ((modMask .|. shiftMask,   xK_c      ), kill                               )

  -- opening program launcher
  , ((modMask ,                xK_p      ), shellPrompt myXPConfig             )

  -- GridSelect
  , ((modMask ,                xK_g      ), goToSelected defaultGSConfig       )

  -- layouts
  , ((modMask,                 xK_space  ), sendMessage NextLayout             )
  , ((modMask .|. shiftMask,   xK_space  ), setLayout $ XMonad.layoutHook conf )

  -- floating layer stuff
  , ((modMask,                 xK_t      ), withFocused $ windows. W.sink      )

  -- focus
  , ((modMask,                 xK_Tab    ), windows W.focusDown                )
  , ((modMask,                 xK_j      ), windows W.focusDown                )
  , ((modMask,                 xK_k      ), windows W.focusUp                  )
  , ((modMask,                 xK_m      ), windows W.focusMaster              )

  -- swapping
  , ((modMask .|. shiftMask,   xK_Return ), windows W.swapMaster               )
  , ((modMask .|. shiftMask,   xK_j      ), windows W.swapDown                 )
  , ((modMask .|. shiftMask,   xK_k      ), windows W.swapUp                   )

  -- increase or decrease number of windows in the master area
  , ((modMask,                 xK_comma  ), sendMessage (IncMasterN 1)         )
  , ((modMask,                 xK_period ), sendMessage (IncMasterN (-1))      )

  -- resizing
  , ((modMask,                 xK_h      ), sendMessage Shrink                 )
  , ((modMask,                 xK_l      ), sendMessage Expand                 )
  , ((modMask .|. shiftMask,   xK_h      ), sendMessage MirrorShrink           )
  , ((modMask .|. shiftMask,   xK_l      ), sendMessage MirrorExpand           )

  -- Programs
  , ((modMask,                 xK_b      ), spawn myBrowser                    )
  , ((modMask,                 xK_o      ), spawn myOffice                     )
  , ((modMask,                 xK_Print  ), spawn myScrot                      )

  -- quit or restart
  , ((modMask .|. shiftMask,   xK_q      ), io (exitWith ExitSuccess)          )
  , ((modMask,                 xK_q      ), spawn myRestart                    )
  -- lockscreen
  , ((controlMask .|. altMask, xK_l      ), spawn myLock                       )
  ]
  ++
  -- mod-[1..9] %! Switch to workspace N
  -- mod-shift-[1..9]] %! Move client to workspace N
  [ ((m .|. modMask, k), windows $ f i)
  | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
  , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
  where
    myBrowser = "$BROWSER"
    myOffice  = "ooffice"
    myScrot   = "scrotinput"
    myLock    = "locks"
    myRestart = "for pid in `pgrep conky`; do kill -9 $pid; done && " ++
                "for pid in `pgrep dzen2`; do kill -9 $pid; done && " ++
                "xmonad --recompile && xmonad --restart"

-- vim:foldmethod=marker foldmarker={{{,}}} filetype=haskell expandtab shiftwidth=4 : 
