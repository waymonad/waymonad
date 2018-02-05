module Waymonad.Utility.Extensible
where

import Control.Monad.IO.Class
import Data.IORef (modifyIORef, readIORef)

import Waymonad (getState)
import Waymonad.Types
import Waymonad.Extensible


modifyStateRef :: (StateMap -> StateMap) -> Way vs a ()
modifyStateRef fun = do
    ref <- wayExtensibleState <$> getState
    liftIO $ modifyIORef ref fun

modifyEState :: ExtensionClass a => (a -> a) -> Way vs b ()
modifyEState = modifyStateRef . modifyValue

setEState :: ExtensionClass a => a -> Way vs b ()
setEState = modifyStateRef . setValue

getEState :: ExtensionClass a => Way vs b a
getEState = do
    state <- liftIO . readIORef . wayExtensibleState =<< getState
    pure $ getValue state

