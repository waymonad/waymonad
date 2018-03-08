module Waymonad.Utility.HaskellSignal
    ( HaskellSignalToken
    , HaskellSignal
    , addHaskellListener
    , makeHaskellSignal
    , removeHaskellListener
    , emitHaskellSignal
    )
where

import Control.Monad.IO.Class (liftIO, MonadIO)
import Data.IntMap (IntMap)
import Data.IORef (IORef, newIORef, readIORef, writeIORef, modifyIORef)

import qualified Data.IntMap as IM

data HaskellSignalToken v m = HaskellSignalToken Int (HaskellSignal v m)
data HaskellSignal v m = HaskellSignal { _hsSignalMap :: IORef (IntMap (v -> m ())), _hsSignalCounter :: IORef Int }

makeHaskellSignal :: MonadIO m => m (HaskellSignal v n)
makeHaskellSignal = liftIO (HaskellSignal <$> newIORef mempty <*> newIORef 0)

addHaskellListener :: MonadIO m => HaskellSignal v n -> (v -> n ()) -> m (HaskellSignalToken v n)
addHaskellListener s@(HaskellSignal mapRef counter) act = liftIO $ do
    value <- readIORef counter
    writeIORef counter (value + 1)

    modifyIORef mapRef (IM.insert value act)
    pure $ HaskellSignalToken value s

removeHaskellListener :: MonadIO m => HaskellSignalToken v n -> m ()
removeHaskellListener (HaskellSignalToken v (HaskellSignal refMap _)) = liftIO $
    modifyIORef refMap (IM.delete v)

emitHaskellSignal :: MonadIO m => v -> HaskellSignal v m -> m ()
emitHaskellSignal v (HaskellSignal mapRef _) = do
    lMap <- liftIO $ readIORef mapRef
    mapM_ ($ v) $ IM.elems lMap
