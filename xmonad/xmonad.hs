import XMonad
import Data.Monoid

import XMonad.Layout.NoBorders
import XMonad.Layout.Fullscreen
import XMonad.Layout.Tabbed
import XMonad.Layout.BinarySpacePartition

import XMonad.Actions.CycleWS
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.DynamicLog

import qualified XMonad.StackSet as W
import qualified Data.Map        as M

import XMonad.Util.Run 

-- color pallette
borderGray   = "#1c1c1c"
borderOrange = "#d65d0e"

textGray   = "#121212"
textOrange = "#d79921"
textAqua   = "#8ec07c"

main = do 
    xmobarPipe <- spawnPipe "xmobar"
    xmonad $ myConfig xmobarPipe

myConfig xmobarPipe = def 
        { terminal = "xterm"
        , logHook  = myXmobarLogHook xmobarPipe
            
        , layoutHook = myLayout

        -- defualts + my extras
        , keys       = keys def `mappend` myKeys
            
        -- on startup make a shell with some fun extras
        , startupHook = spawn "xterm /home/ls_brillant/.special_start"
        -- REMEBER this is needed for some reason.
        , handleEventHook = mempty <+> docksEventHook
        -- colors
        , normalBorderColor  = borderGray
        , focusedBorderColor = borderOrange
        }

myXmobarLogHook xmobarPipe = dynamicLogWithPP xmobarPrinter 
    where 
      xmobarPrinter = def 
        { ppOutput  = hPutStrLn xmobarPipe
        -- wrap current workspace in square brackets
        , ppCurrent = xmobarColor textOrange "" . wrap "[" "]"
        -- show title of the currently running program
        , ppTitle   = xmobarColor textAqua   "" . shorten 40 
        }

myLayout = avoidStruts (tiled ||| emptyBSP ||| tabbed shrinkText tabConfig) ||| noBorders (fullscreenFull Full)
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
                     , activeColor         = borderGray
                     , inactiveBorderColor = borderGray
                     , inactiveColor       = textGray 
                     }

myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $
    -- launch firefox
    [ ((modm .|. shiftMask,  xK_f        ), spawn "firefox") 
    
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

