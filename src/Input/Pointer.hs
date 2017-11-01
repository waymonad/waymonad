module Input.Pointer
where

import Foreign.Ptr (Ptr)
import Graphics.Wayland.WlRoots.Input (InputDevice)
import Graphics.Wayland.WlRoots.Input.Pointer (WlrPointer)
import Graphics.Wayland.WlRoots.Cursor (WlrCursor, attachInputDevice)

handlePointer :: Ptr WlrCursor -> Ptr InputDevice -> Ptr WlrPointer -> IO ()
handlePointer cursor dev _ = do
    attachInputDevice cursor dev
