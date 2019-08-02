import XMonad
import Data.Monoid

-- Layouts
import XMonad.Layout.NoBorders
import XMonad.Layout.Fullscreen
import XMonad.Layout.Tabbed
import XMonad.Layout.Circle

-- For Xmobar and stuff
import XMonad.Actions.CycleWS
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.DynamicLog

import qualified XMonad.StackSet as W
import qualified Data.Map        as M

import XMonad.Util.Run
import XMonad.Util.NamedScratchpad
import XMonad.ManageHook

import System.Environment (getEnv) 
import Data.List (isInfixOf)

--------------------
-- Color Pallette --
--------------------
borderGray   = "#1c1c1c"
borderOrange = "#d79921"
--borderOrange = "#d65d0e"

textGray   = "#121212"
textOrange = "#d79921"
textAqua   = "#8ec07c"


----------------
-- Workspaces --
----------------
webWS  = "<icon=web.xbm/>" 
mailWS = "<icon=letter.xbm/>"
termWS = "<icon=term.xbm/>"

myWorkspaces :: [String]
myWorkspaces = [webWS, mailWS, termWS] ++ (map (show) [4..9])


main = do
    homeDir <- getEnv "HOME"
    xmobarPipe <- spawnPipe $ "xmobar "++homeDir++"/.config/xmobar/xmobarrc"
    xmonad $ myConfig xmobarPipe homeDir

myConfig xmobarPipe homeDir = def 
        { terminal = "urxvt"
        , logHook  = myXmobarLogHook xmobarPipe
            
        , layoutHook = myLayout
        , manageHook = myManageHook

        , workspaces = myWorkspaces
        , borderWidth = 2
         -- defualts + my extras
        , keys       = myKeys <> (keys def)
            
        -- on startup make a shell with some fun extras
        -- , startupHook = spawn $ "xterm " ++ homeDir ++ "/.special_start"
        -- REMEBER this is needed for some reason.
        , handleEventHook = mempty <+> docksEventHook
        -- colors
        , normalBorderColor  = borderGray
        , focusedBorderColor = borderOrange
        }

myXmobarLogHook xmobarPipe = (dynamicLogWithPP . namedScratchpadFilterOutWorkspacePP) $ xmobarPrinter 
    where 
      xmobarPrinter = def 
        { ppOutput  = hPutStrLn xmobarPipe
        -- wrap current workspace in square brackets
        , ppCurrent = xmobarColor "#282828" textOrange . pad
        , ppOrder = \(ws:l:_:_) -> [ws,l] -- workspace, layout
        , ppSep = " | " 
        , ppLayout = layoutPP
        }
      layoutPP layout = case layout of
                          "Tabbed Simplest" -> "Tabbed"
                          otherwise         -> layout

myLayout = avoidStruts (tiled ||| Circle ||| tabbed shrinkText tabConfig) ||| noBorders (fullscreenFull Full)
  where
     -- default tiling algorithm partitions the screen into two panes
     tiled   = Tall nmaster delta ratio
     -- The default number of windows in the master pane
     nmaster = 1
     -- Default proportion of screen occupied by master pane
     ratio   = 1/2
     -- Percent of screen to increment by when resizing panes
     delta   = 3/100
     -- tab colors
     tabConfig = def { activeBorderColor   = borderOrange 
                     , activeColor         = borderOrange
                     , inactiveBorderColor = borderGray
                     , inactiveColor       = textGray 
                     }


scratchpads = 
    [ NS "smterm" "urxvt -name ScratchPad:smterm" (resource =? "ScratchPad:smterm")  (customFloating $ W.RationalRect (1/6) (1/6) (2/3) (2/3))  
    , NS "mixer" "urxvt -e alsamixer" (title =? "alsamixer")  (customFloating $ W.RationalRect (1/6) (1/6) (2/3) (2/3))  
    ]


myManageHook = (composeAll . concat $ [
        [ className =? c --> doFloat | c <- classNameFloats],
        [ title =? t --> doFloat | t <- titleFloats]
    ]) <+> namedScratchpadManageHook scratchpads
    where classNameFloats = ["Shutter", "Burp Suite Professional"]
          titleFloats = ["xmessage"]


myKeys conf@(XConfig {XMonad.modMask = modm, XMonad.terminal = term}) = M.fromList $
    -- launch firefox
    [ ((modm .|. shiftMask,  xK_f        ), spawn "firefox")
    -- rofi is a nice launcher 
    , ((modm              ,  xK_p        ), spawn ("rofi -terminal "++term++" -show"))

    -- Nice workspace switching
    , ((modm               , xK_minus    ), prevWS)
    , ((modm .|. shiftMask , xK_minus    ), shiftToPrev >> prevWS)
    , ((modm               , xK_equal    ), nextWS)
    , ((modm .|. shiftMask , xK_equal    ), shiftToNext >> nextWS)

    -- Audio controls
    -- mute
    , ((0, 0x1008ff12), spawn "amixer -q set Master toggle")
    -- volume up
    , ((0, 0x1008ff13), spawn "amixer -q set Master 5%+")
    -- volume down
    , ((0, 0x1008ff11), spawn "amixer -q set Master 5%-")

    -- Lock Computer
    , ((modm .|. shiftMask , xK_l), spawn "dm-tool lock")
    

    -- My ScratchPads
    , ((modm               , xK_t), namedScratchpadAction scratchpads "smterm")
    , ((modm .|. shiftMask , xK_m), namedScratchpadAction scratchpads "mixer")

    -- Toggle the status bar gap
    -- Use this binding with avoidStruts from Hooks.ManageDocks.
    , ((modm              , xK_b     ), sendMessage ToggleStruts)
    ]

    ++
    --
    -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
    --
    [((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
       | (key, sc) <- zip [xK_e, xK_w, xK_r] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]

