{-- xmonad.hs | works only with xmonad-darcs and xmonad-contrib-darcs (04-06-10)
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

-- Main --
main = do
  xmproc <- spawnPipe "xmobar"
  xmonad  $ withUrgencyHook NoUrgencyHook $ defaultConfig
    { manageHook            = myManageHook
    , layoutHook            = myLayoutHook 
    , borderWidth           = myBorderWidth
    , normalBorderColor     = myNormalBorderColor
    , focusedBorderColor    = myFocusedBorderColor
    , keys                  = myKeys
    , logHook               = myLogHook xmproc
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
  , [ className =? c                     --> doCenterFloat    |  c    <- myCenterFloats ]
  , [ className =? c                     --> doShift "2:web"  |  c    <- myWebs         ]
  , [ className =? c                     --> doShift "4:pdf"  |  c    <- myPdf          ]
  , [ className =? c                     --> doShift "5:doc"  |  c    <- myDocs         ]
  , [ className =? c                     --> doShift "6:vbox" |  c    <- myVirt         ]
  , [ className =? c                     --> doShift "8:vid"  |  c    <- myVid          ]
  , [ className =? c                     --> doShift "9:gimp" |  c    <- myGimp         ]
  ] ) <+> manageDocks
  
  where

    role           = stringProperty "WM_WINDOW_ROLE"
    name           = stringProperty "WM_NAME"

    -- classnames
    myCenterFloats = ["Xmessage","Zenity","feh","GQview"]   ++
                     ["XFontSel","Save As...","MPlayer"]

    myWebs         = ["Namoroka","Firefox","Google-chrome"] ++
                     ["Chromium"]

    myDocs         = ["OpenOffice.org"]

    myGimp         = ["Gimp"]

    myVid          = ["MPlayer","VLC media player"]

    myVirt         = ["VirtualBox"]

    myPdf          = ["Evince"]

-- a trick for fullscreen but stil allow focusing of other WSs
myDoFullFloat :: ManageHook
myDoFullFloat = doF W.focusDown <+> doFullFloat

--logHook
myLogHook :: Handle -> X ()
myLogHook h = dynamicLogWithPP $ customPP { ppOutput = hPutStrLn h }

---- Looks --
---- bar
customPP :: PP
customPP = defaultPP
  { ppHidden  = xmobarColor "#00FF00" ""
  , ppCurrent = xmobarColor "#FF0000" "" . wrap "[" "]"
  , ppUrgent  = xmobarColor "#FF0000" "" . wrap "*" "*"
  , ppLayout  = xmobarColor "#FF0000" ""
  , ppTitle   = xmobarColor "#00FF00" "" . shorten 80
  , ppSep     = "<fc=#0033FF> | </fc>"
  }

-- some nice colors for the prompt windows to match the xmobar status bar.
myXPConfig = defaultXPConfig
  { font     = "-*-profont-*-*-*-*-11-*-*-*-*-*-*-*"
  , fgColor  = "#00FFFF"
  , bgColor  = "#000000"
  , bgHLight = "#000000"
  , fgHLight = "#FF0000"
  , position = Top
  }
 
--- My Theme For Tabbed layout
myTheme = defaultTheme 
  { decoHeight          = 16
  , activeColor         = "#a6c292"
  , activeBorderColor   = "#a6c292"
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
myWorkspaces = ["1:chat", "2:web", "3:code", "4:pdf", "5:doc", "6:vbox", "7:games", "8:vid", "9:gimp"]

-- Switch to the "web" workspace
viewWeb = windows (W.greedyView "2:web") -- (0,0a)
--

-- keys
myKeys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
myKeys conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $

  -- kiling programs
  [ ((modMask ,              xK_Return ), spawn $ XMonad.terminal conf       )
  , ((modMask .|. shiftMask, xK_c      ), kill                               )

  -- opening program launcher
  , ((modMask ,              xK_p      ), shellPrompt myXPConfig             )

  -- GridSelect
  , ((modMask ,              xK_g      ), goToSelected defaultGSConfig       )

  -- layouts
  , ((modMask,               xK_space  ), sendMessage NextLayout             )
  , ((modMask .|. shiftMask, xK_space  ), setLayout $ XMonad.layoutHook conf )

  -- floating layer stuff
  , ((modMask,               xK_t      ), withFocused $ windows. W.sink      )

  -- focus
  , ((modMask,               xK_Tab    ), windows W.focusDown                )
  , ((modMask,               xK_j      ), windows W.focusDown                )
  , ((modMask,               xK_k      ), windows W.focusUp                  )
  , ((modMask,               xK_m      ), windows W.focusMaster              )

  -- swapping
  , ((modMask .|. shiftMask, xK_Return ), windows W.swapMaster               )
  , ((modMask .|. shiftMask, xK_j      ), windows W.swapDown                 )
  , ((modMask .|. shiftMask, xK_k      ), windows W.swapUp                   )

  -- increase or decrease number of windows in the master area
  , ((modMask,               xK_comma  ), sendMessage (IncMasterN 1)         )
  , ((modMask,               xK_period ), sendMessage (IncMasterN (-1))      )

  -- resizing
  , ((modMask,               xK_h      ), sendMessage Shrink                 )
  , ((modMask,               xK_l      ), sendMessage Expand                 )
  , ((modMask .|. shiftMask, xK_h      ), sendMessage MirrorShrink           )
  , ((modMask .|. shiftMask, xK_l      ), sendMessage MirrorExpand           )

  -- Programs
  , ((modMask,               xK_b      ), spawn "/usr/bin/google-chrome"     )
  , ((modMask,               xK_o      ), spawn "/usr/bin/ooffice"           )
  , ((modMask,               xK_Print  ), spawn "/home/scripts/scrotinput"   )

  -- quit or restart
  , ((modMask .|. shiftMask, xK_q      ), io (exitWith ExitSuccess)          )
  , ((modMask,               xK_q      ), restart "xmonad" True              )
  ]
  ++
  -- mod-[1..9] %! Switch to workspace N
  -- mod-shift-[1..9]] %! Move client to workspace N
  [((m .|. modMask, k), windows $ f i)
    | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
    , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
