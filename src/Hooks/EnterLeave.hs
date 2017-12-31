module Hooks.EnterLeave
    ( enterLeaveHook
    )
where

import Control.Monad (forM_)
import Control.Monad.IO.Class (liftIO)

import Graphics.Wayland.WlRoots.Surface (surfaceSendLeave, surfaceSendEnter)

import Output (Output (..))
import Utility (whenJust, doJust)
import View (getViewSurface)
import ViewSet (Workspace (..), WSTag)
import WayUtil.Focus (OutputMappingEvent (..))
import Waymonad (getEvent, SomeEvent, getWorkspace)
import Waymonad.Types (Way)

sendLeaves
    :: WSTag a
    => Output
    -> a
    -> Way a ()
sendLeaves output ws = doJust (wsViews <$> getWorkspace ws) $ \zipper ->
    liftIO $ forM_ zipper $ \view ->
        doJust (getViewSurface view) (flip surfaceSendLeave $ outputRoots output)

sendEnters
    :: WSTag a
    => Output
    -> a
    -> Way a ()
sendEnters output ws = doJust (wsViews <$> getWorkspace ws) $ \zipper ->
    liftIO $ forM_ zipper $ \view ->
        doJust (getViewSurface view) (flip surfaceSendEnter $ outputRoots output)

outputChangeEvt
    :: WSTag a
    => Maybe (OutputMappingEvent a)
    -> Way a ()
outputChangeEvt Nothing = pure ()
outputChangeEvt (Just evt) = do
    whenJust (outputMappingEvtPre evt) (sendLeaves $ outputMappingEvtOutput evt)
    whenJust (outputMappingEvtPre evt) (sendEnters $ outputMappingEvtOutput evt)


enterLeaveHook :: WSTag a => SomeEvent -> Way a ()
enterLeaveHook = outputChangeEvt . getEvent
