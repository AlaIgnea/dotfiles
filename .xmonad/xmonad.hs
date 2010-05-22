{-- xmonad.hs | 
Original Author: Jelle van der Waa ( jelly12gen )

modified by keksvernichter @ gmail.com ( AlaIgnea )
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
 
-- utils
import XMonad.Util.Run(spawnPipe)
import qualified XMonad.Prompt as P
import XMonad.Prompt.Shell
import XMonad.Prompt
import XMonad.Prompt.Workspace
 
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
 
-- Main --
main = do
	d <- spawnPipe myStatusBar
	spawn myOtherBar
	xmonad $ withUrgencyHook NoUrgencyHook $ defaultConfig
		{ layoutHook            = myLayoutHook
		, manageHook            = myManageHook
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
myManageHook = composeAll . concat $ 
                [ [ isFullscreen                      --> doFullFloat ]
                , [ className =? a                    --> doCenterFloat | a <- myCenterFloats ] 
                , [ moveToC "Gimp"                    "gimp" ]
                , [ moveToC "Namoroka"                "web" ]
                , [ moveToC "MPlayer"                 "vid" ]
                , [ moveToC "VirtualBox"              "virtual" ]
                , [ moveToC "Evince"                  "pdf" ] ]
				++
				[ [ resource  =? a --> doCenterFloat  | a <- myCenterFloats ] ]
                where
                    myIgnores                         = ["trayer"]
                    myCenterFloats                    = ["Xmessage","Zenity","GQview","Pokerth"]
                    myOtherFloats                     = []
                    moveToC c w                       = className =? c --> doF (W.shift w)
 
 
-- dzen options are external except the font and the colors
myFont="-*-terminus-*-r-normal-*-12-*-*-*-*-*-iso8859-*"
myStatusBar = "dzen2 `cat ~/.dzen2_l` -fg '#606060' -bg '#303030' -e 'onexit=ungrabmouse'  -fn " ++ myFont
myOtherBar = "conky -c ~/.dzen_conkyrc | dzen2 `cat ~/.dzen2_r` -fg '#606060' -bg '#303030' -fn " ++ myFont

--logHook
myLogHook :: Handle -> X ()
myLogHook h = dynamicLogWithPP $ customPP
	{ ppCurrent = dzenColor "#303030" "#909090" . pad
	, ppHidden  = dzenColor "#909090" "" . pad
	, ppLayout  = dzenColor "#909090" "" . pad
	, ppUrgent  = wrap (dzenColor "#ff0000" "" "{") (dzenColor "#ff0000" "" "}") . pad
	, ppTitle   = wrap "^fg(#909090)[ " " ]^fg()" . shorten 40 
	, ppWsSep   = ""
	, ppSep     = "  "
	, ppOutput  = hPutStrLn h
	}

---- Looks --
---- bar
customPP :: PP
customPP = defaultPP
	{ ppHidden  = dzenColor "#606060" ""
	, ppCurrent = dzenColor "#303030" "#909090" . wrap "[" "]"
	, ppUrgent  = dzenColor "#ff0000" "" . wrap "*" "*"
	, ppLayout  = dzenColor "#909090" ""
	, ppTitle   = dzenColor "#909090" "" . shorten 80
	, ppSep     = "<fc=#0033FF> | </fc>"
	}

-- some nice colors for the prompt windows to match the dzen status bar.
myXPConfig = defaultXPConfig
    { font     = "-*-terminus-*-*-*-*-12-*-*-*-*-*-*-u"
	, fgColor  = "#303030"
	, bgColor  = "#909090"
	, bgHLight = "#606060"
	, fgHLight = "#303030"
	, position = Top
    }
 
--- My Theme For Tabbed layout
myTheme = defaultTheme 
	{ decoHeight          = 16
	, activeColor         = "#303030"
	, activeBorderColor   = "#909090"
	, activeTextColor     = "#000000"
	, inactiveBorderColor = "#000000"
	}
 
--LayoutHook
myLayoutHook = avoidStruts $ onWorkspace "web" webL $
                             onWorkspace "irc" webL $ 
                             onWorkspace "virtual" fullL $ 
                             onWorkspace "vid" fullL $ 
                             onWorkspace "gimp" gimpL $ 
                             standardLayouts
    where
        standardLayouts = tiled ||| reflectTiled ||| Mirror tiled ||| Full
 
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
myNormalBorderColor  = "#303030"
myFocusedBorderColor = "#FF0000"
--
  
--Workspaces
myWorkspaces :: [WorkspaceId]
myWorkspaces = 	map show [1..9] ++
                [ "web"
                , "irc"
                , "pdf"
                , "doc"
                , "virtual"
                , "vid"
                , "gimp"]
--
 
-- Switch to the "web" workspace
viewWeb = windows (W.greedyView "web") -- (0,0a)
--
 
-- keys
myKeys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
myKeys conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $

    [ ((modMask,      xK_b),                                 sendMessage ToggleStruts) -- layouts
    , ((modMask,      xK_c),                                  spawn "/usr/bin/oocalc") -- OpenOffice Calc
    , ((modMask,      xK_f),                                 spawn "/usr/bin/firefox") -- firefox
    , ((modMask,      xK_h),                                       sendMessage Shrink) -- shrink term
    , ((modMask,      xK_k),                                        windows W.focusUp) -- prev terminal
    , ((modMask,      xK_l),                                       sendMessage Expand) -- expand term
    , ((modMask,      xK_m),                                    windows W.focusMaster) -- new master
    , ((modMask,      xK_n),                                                  refresh) -- refresh
    , ((modMask,      xK_o),                                spawn "/usr/bin/oowriter") -- OpenOffice Writer
    , ((modMask,      xK_p),                                   shellPrompt myXPConfig) -- program launcher
    , ((modMask,      xK_q),                                         spawn myRestart ) -- reload config
    , ((modMask,      xK_t),                           withFocused $ windows . W.sink) -- floating layer stuff
    , ((modMask,      xK_z),             workspacePrompt myXPConfig (windows .W.view)) -- workspace prompt
    , ((modMask,    xK_Tab),                                      windows W.focusDown) -- next terminal
    , ((modMask,  xK_space),                                   sendMessage NextLayout) -- layouts
    , ((modMask,  xK_comma),                               sendMessage (IncMasterN 1)) -- increase term in master area
    , ((modMask,  xK_Print),                         spawn "/home/scripts/scrotinput") -- screenshot
    , ((modMask, xK_period),                            sendMessage (IncMasterN (-1))) -- decrease term in master area
    , ((modMask, xK_Return),                             spawn $ XMonad.terminal conf) -- launching new Terminal
	
    , ((modMask .|. shiftMask, xK_c),                                            kill) -- kill window
    , ((modMask .|. shiftMask, xK_h),                        sendMessage MirrorShrink) -- mirrorshrink
    , ((modMask .|. shiftMask, xK_j),                             windows W.swapDown ) -- swap down
    , ((modMask .|. shiftMask, xK_k),                               windows W.swapUp ) -- swap up
    , ((modMask .|. shiftMask, xK_l),                        sendMessage MirrorExpand) -- mirrorexpand
    , ((modMask .|. shiftMask, xK_q),                       io (exitWith ExitSuccess)) -- quit
    , ((modMask .|. shiftMask, xK_s),     spawn "/usr/bin/sudo /sbin/shutdown -h now") -- shutdown
    , ((modMask .|. shiftMask, xK_z),  workspacePrompt myXPConfig (windows . W.shift)) -- mv term to workspace
    , ((modMask .|. shiftMask, xK_space),          setLayout $ XMonad.layoutHook conf) -- layouts
    , ((modMask .|. shiftMask, xK_Return),                       windows W.swapMaster) -- swap master
    ]
    ++
    [((m .|. modMask, k), windows $ f i)                                               -- switch to workspace
    | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]                            -- move term to workspace
    , (f, m) <- [(W.greedyView, 0),  (W.shift, shiftMask)]]
    where
        myRestart = "for pid in `pgrep conky`; do kill -9 $pid; done && " ++           -- killall conky/dzen2 if running
                    "for pid in `pgrep dzen2`; do kill -9 $pid; done && " ++           -- before executing reload
                    "xmonad --recompile && xmonad --restart"
