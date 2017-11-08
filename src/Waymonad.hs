{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Waymonad
    ( get
    , modify

    , WayStateRef
    , WayState
    , runWayState
    , runWayState'

    , LayoutCacheRef
    , LayoutCache
    , runLayoutCache
    , runLayoutCache'

    , viewBelow
    , floatBelow

    , KeyBinding
    , BindingMap
    , LogFun

    , WayBindingState (..)
    , runWayBinding

    , Way
    , getState
    , getSeat
    , runWay
    , makeCallback
    , setCallback
    , withSeat

    , WayLoggers (..)
    , Logger (..)
    , getLoggers
    )
where

import System.IO.Unsafe (unsafeInterleaveIO)

import Control.Applicative ((<|>))
import Control.Monad (forM)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (ReaderT(..), MonadReader(..), local)
import Control.Monad.Trans.Class (MonadTrans (..))
import Data.IORef (IORef, modifyIORef, readIORef)
import Data.Map (Map)
import Data.Set (Set)
import Data.Text (Text)
import Data.Typeable (Typeable, typeOf)
import Data.Word (Word32)
import Foreign.Ptr (Ptr)

import Graphics.Wayland.WlRoots.Box (Point (..), WlrBox)
import Graphics.Wayland.WlRoots.Seat (WlrSeat)

import Config (WayConfig)
import View (View, getViewEventSurface)
import qualified ViewSet as VS
import Waymonad.Extensible

import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import qualified Data.Set as S


-- All of this makes for a fake `Monad State` in IO
-- We need this because we run into callbacks *a lot*.
-- We have to preserve/modify state around those, which cannot be
-- done with the normal StateT (since we exit our Monad-Stack all the time)
-- but we are in IO, which can be abused with this trick.
-- It should all be hidden in the high level apis, low level APIs will
-- require the get and runWayState around callbacks that are IO
type WayStateRef a = IORef (VS.ViewSet a)

type LayoutCacheRef = IORef (IntMap [(View, WlrBox)])

newtype LayoutCache a = LayoutCache (ReaderT (LayoutCacheRef) IO a)
    deriving (Functor, Applicative, Monad, MonadIO, MonadReader LayoutCacheRef)

newtype WayState a b = WayState (ReaderT (WayStateRef a) IO b)
    deriving (Functor, Applicative, Monad, MonadIO, MonadReader (WayStateRef a))

get :: (MonadReader (IORef a) m, MonadIO m) => m a
get = liftIO . readIORef =<< ask

modify :: (MonadReader (IORef a) m, MonadIO m) => (a -> a) -> m ()
modify fun = do
    ref <- ask
    liftIO $ modifyIORef ref fun

runWayState :: MonadIO m =>  WayState a b -> WayStateRef a -> m b
runWayState (WayState m) ref = liftIO $ runReaderT m ref

runWayState' :: MonadIO m => WayStateRef a -> WayState a b -> m b
runWayState' ref act = runWayState act ref

runLayoutCache :: MonadIO m => LayoutCache a -> LayoutCacheRef -> m a
runLayoutCache (LayoutCache m) ref = liftIO $ runReaderT m ref

runLayoutCache' :: MonadIO m => LayoutCacheRef -> LayoutCache a -> m a
runLayoutCache' ref act = runLayoutCache act ref

floatBelow
    :: Point
    -> Way a (Maybe View)
floatBelow (Point x y) = do
    views <- liftIO . fmap S.toList . readIORef . wayFloating =<< getState
    candidates <- liftIO $ forM views $
        \view -> unsafeInterleaveIO $ (fmap . fmap) (const view) $
            getViewEventSurface view (fromIntegral x) (fromIntegral y)

    pure $ foldl (<|>) Nothing candidates

viewBelow
    :: Point
    -> Int
    -> Way a (Maybe View)
viewBelow point ws = do
    fullCache <- liftIO . readIORef . wayBindingCache =<< getState
    case IM.lookup ws fullCache of
        Nothing -> pure Nothing
        Just x -> liftIO $ VS.viewBelow point x

data Logger = Logger
    { loggerActive :: Bool
    , loggerName :: Text
    }

data WayLoggers = WayLoggers
    { loggerOutput :: Logger
    , loggerWS :: Logger
    , loggerFocus :: Logger
    , loggerXdg :: Logger
    , loggerKeybinds :: Logger
    , loggerSpawner :: Logger
    }

data WayBindingState a = WayBindingState
    { wayBindingCache :: LayoutCacheRef
    , wayBindingState :: WayStateRef a
    , wayBindingCurrent :: IORef [(Ptr WlrSeat, Int)]
    , wayBindingMapping :: IORef [(a, Int)]
    , wayBindingOutputs :: IORef [Int]
    , wayBindingSeats   :: IORef [Ptr WlrSeat]
    , wayLogFunction :: LogFun a
    , wayExtensibleState :: IORef StateMap
    , wayConfig :: WayConfig
    , wayFloating :: IORef (Set View)
    }

newtype WayLogging a = WayLogging (ReaderT WayLoggers IO a)
    deriving (Functor, Applicative, Monad, MonadIO, MonadReader WayLoggers)

newtype WayBinding a b = WayBinding (ReaderT (WayBindingState a) WayLogging b)
    deriving (Functor, Applicative, Monad, MonadIO, MonadReader (WayBindingState a))

type KeyBinding a = Way a ()
type BindingMap a = Map (Word32, Int) (KeyBinding a)
type LogFun a = Way a ()

newtype Way a b = Way (ReaderT (Maybe (Ptr WlrSeat)) (WayBinding a) b)
    deriving (Functor, Applicative, Monad, MonadIO, MonadReader (Maybe (Ptr WlrSeat)))

instance forall a b. (Typeable a, Typeable b) => Show (Way a b) where
    show =  show . typeOf

getLoggers :: Way a WayLoggers
getLoggers = Way $ lift getLoggers'
    where   getLoggers' :: WayBinding a WayLoggers
            getLoggers' = WayBinding $ lift ask

getState :: Way a (WayBindingState a)
getState = Way $ lift ask

getSeat :: Way a (Maybe (Ptr WlrSeat))
getSeat = do
    current <- ask
    case current of
        Just _ -> pure current
        Nothing -> do
            seats <- liftIO . readIORef . wayBindingSeats =<< getState
            pure $ case seats of
                [x] -> Just x
                _ -> Nothing

runWayLogging :: MonadIO m => WayLoggers -> WayLogging a -> m a
runWayLogging val (WayLogging act) = liftIO $ runReaderT act val

runWayBinding :: MonadIO m => WayLoggers -> WayBindingState a -> WayBinding a b -> m b
runWayBinding logger val (WayBinding act) =
    liftIO $ runWayLogging logger $ runReaderT act val


runWay
    :: MonadIO m
    => Maybe (Ptr WlrSeat)
    -> WayBindingState a
    -> WayLoggers
    -> Way a b
    -> m b
runWay seat state logger (Way m) =
    liftIO $ runWayBinding logger state $ runReaderT m seat

makeCallback :: (c -> Way a b) -> Way a (c -> IO b)
makeCallback act = do
    seat <- getSeat
    state <- getState
    loggers <- getLoggers
    pure (\arg -> runWay seat state loggers (act arg))

setCallback :: (c -> Way a b) -> ((c -> IO b) -> IO d) -> Way a d
setCallback act fun = do
    ioAct <- makeCallback act
    liftIO $ fun ioAct


withSeat :: Maybe (Ptr WlrSeat) -> Way a b -> Way a b
withSeat seat (Way m) = Way $ local (const seat) m
