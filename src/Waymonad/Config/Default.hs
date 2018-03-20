{-# LANGUAGE NumDecimals #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Waymonad.Config.Default (defaultConfig)
where

import Data.Semigroup ((<>))
import Data.String (IsString)
import Data.Text (Text)
import Text.XkbCommon.InternalTypes (Keysym(..))
import Text.XkbCommon.KeysymList

import Graphics.Wayland.WlRoots.Input.Keyboard (WlrModifier(..))
import Graphics.Wayland.WlRoots.Render.Color (Color (..))

import Waymonad (KeyBinding)
import Waymonad.Actions.Spawn (spawn, manageSpawnOn)
import Waymonad.Actions.Spawn.X11 (manageX11SpawnOn)
import Waymonad.Actions.Startup.Environment
import Waymonad.GlobalFilter
import Waymonad.Hooks.EnterLeave (enterLeaveHook)
import Waymonad.Hooks.FocusFollowPointer
import Waymonad.Hooks.KeyboardFocus
import Waymonad.Hooks.ScaleHook
import Waymonad.IdleDPMS
import Waymonad.IdleManager
import Waymonad.Input (attachDevice)
import Waymonad.Input.Cursor.Bindings (makeDefaultMappings)
import Waymonad.Layout.Choose
import Waymonad.Layout.Mirror (mkMirror, ToggleMirror (..))
import Waymonad.Layout.Ratio 
import Waymonad.Layout.SmartBorders
import Waymonad.Layout.Spiral 
import Waymonad.Layout.Tall 
import Waymonad.Layout.ToggleFull (mkTFull, ToggleFullM (..))
import Waymonad.Layout.TwoPane 
import Waymonad.Main (WayUserConf (..))
import Waymonad.Navigation2D
import Waymonad.Output (Output (outputRoots), addOutputToWork, setPreferdMode)
import Waymonad.Protocols.GammaControl
import Waymonad.Protocols.IdleInhibit
import Waymonad.Protocols.LinuxDMABuf
import Waymonad.Protocols.Screenshooter
import Waymonad.Types (WayHooks (..))
import Waymonad.Utility (sendMessage, focusNextOut, sendTo, closeCurrent, closeCompositor)
import Waymonad.Utility.Floating (centerFloat)
import Waymonad.Utility.Timing
import Waymonad.Utility.View
import Waymonad.Utility.ViewSet (modifyFocusedWS)
import Waymonad.ViewSet (WSTag, Layouted, FocusCore, ListLike (..))
import Waymonad.ViewSet.XMonad (ViewSet, sameLayout)

import qualified Waymonad.Hooks.OutputAdd as H
import qualified Waymonad.Hooks.SeatMapping as SM
-- The Shells
import qualified Waymonad.Shells.XWayland as XWay
import qualified Waymonad.Shells.XdgShell as Xdg
import qualified Waymonad.Shells.XdgShellv6 as Xdgv6
import qualified Waymonad.Shells.WlShell as Wl

wsSyms :: [Keysym]
wsSyms =
    [ keysym_1
    , keysym_2
    , keysym_3
    , keysym_4
    , keysym_5
    , keysym_6
    , keysym_7
    , keysym_8
    , keysym_9
    , keysym_0
    ]

workspaces :: IsString a => [a]
workspaces = ["1", "2", "3", "4", "5", "6", "7", "8", "9", "0"]

bindings :: (Layouted vs ws, ListLike vs ws, FocusCore vs ws, IsString ws, WSTag ws)
         => WlrModifier -> [(([WlrModifier], Keysym), KeyBinding vs ws)]
bindings modi =
    [ (([modi], keysym_k), moveUp)
    , (([modi], keysym_j), moveDown)
    , (([modi], keysym_h), moveLeft)
    , (([modi], keysym_l), moveRight)
    , (([modi, Shift], keysym_k), modifyFocusedWS $ flip _moveFocusedLeft )
    , (([modi, Shift], keysym_j), modifyFocusedWS $ flip _moveFocusedRight)
    , (([modi], keysym_f), sendMessage ToggleFullM)
    , (([modi], keysym_m), sendMessage ToggleMirror)
    , (([modi], keysym_space), sendMessage NextLayout)


    , (([modi], keysym_Left), sendMessage $ DecreaseRatio 0.1)
    , (([modi], keysym_Right), sendMessage $ IncreaseRatio 0.1)

    , (([modi], keysym_Return), spawn "weston-terminal")
    , (([modi], keysym_d), spawn "rofi -show run")

    , (([modi], keysym_o), centerFloat)

    , (([modi], keysym_n), focusNextOut)
    , (([modi], keysym_q), closeCurrent)
    , (([modi, Shift], keysym_e), closeCompositor)
    ] ++ concatMap (\(sym, ws) -> [(([modi], sym), greedyView ws), (([modi, Shift], sym), sendTo ws), (([modi, Ctrl], sym), copyView ws)]) (zip wsSyms workspaces)

defaultConfig :: WlrModifier -> WayUserConf (ViewSet Text) Text
defaultConfig modi = WayUserConf
    { wayUserConfWorkspaces   = workspaces
    , wayUserConfLayouts      = sameLayout . mkSmartBorders 2. mkTFull . mkMirror $ (Tall 0.5 ||| TwoPane 0.5 ||| Spiral 0.618)
    , wayUserConfManagehook   = XWay.overrideXRedirect <> manageSpawnOn <> manageX11SpawnOn
    , wayUserConfEventHook    = idleDPMSHandler
    , wayUserConfKeybinds     = bindings modi
    , wayUserConfPointerbinds = makeDefaultMappings modi

    , wayUserConfInputAdd    = flip attachDevice "seat0"
    , wayUserConfDisplayHook =
        [ getGammaBracket
        , getFilterBracket filterUser
        , baseTimeBracket
        , envBracket [ ("QT_QPA_PLATFORM", "wayland-egl")
                     -- breaks firefox (on arch) :/
                     --, ("GDK_BACKEND", "wayland")
                     , ("SDL_VIDEODRIVER", "wayland")
                     , ("CLUTTER_BACKEND", "wayland")
                     ]
        , getIdleInihibitBracket
        ]
    , wayUserConfBackendHook = [getIdleBracket 6e5 {- 10 minutes in ms -}, getLinuxDMABufBracket]
    , wayUserConfPostHook    = [getScreenshooterBracket]
    , wayUserConfCoreHooks   = WayHooks
        { wayHooksVWSChange       = wsScaleHook
        , wayHooksOutputMapping   = enterLeaveHook <> handlePointerSwitch <> SM.mappingChangeEvt
        , wayHooksSeatWSChange    = SM.wsChangeLogHook <> handleKeyboardSwitch
        , wayHooksSeatOutput      = SM.outputChangeEvt <> handlePointerPull
        , wayHooksSeatFocusChange = focusFollowPointer
        , wayHooksNewOutput       = H.outputAddHook
        }
    , wayUserConfShells = [Xdg.makeShell, Xdgv6.makeShell, Wl.makeShell, XWay.makeShell]
    , wayUserConfLog = pure ()
    , wayUserConfOutputAdd = \out -> setPreferdMode (outputRoots out) $
        addOutputToWork out Nothing
    , wayUserconfLoggers = Nothing
    , wayUserconfColor = Color 0.5 0 0 1
    , wayUserconfColors = mempty
    , wayUserconfFramerHandler = Nothing -- Just $ damageDisplay 30
    }

