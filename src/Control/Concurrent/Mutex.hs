module Control.Concurrent.Mutex (
                                -- * Mutexes
                                  Mutex
                                , newMutex
                                , acquireMutex
                                , releaseMutex
                                , acquireMutexIO
                                , releaseMutexIO
                                , runHeld
                                -- * Locked variables
                                , Locked
                                , newWithLock
                                , lock
                                , unlock
                                , runLocked
                                , modifyLocked
                                , modifyLockedM
                                , modifyLockedWith
                                , modifyLockedWithM
                                , readLocked
                                ) where

import Prelude ()
import BasicPrelude

import Control.Concurrent.STM

liftSTM :: MonadIO m => STM a -> m a
liftSTM = liftIO . atomically

newtype Mutex = Mutex (TMVar ())

newMutex :: STM Mutex
newMutex = Mutex <$> newTMVar ()

acquireMutex :: Mutex -> STM ()
acquireMutex (Mutex m) = takeTMVar m

releaseMutex :: Mutex -> STM ()
releaseMutex (Mutex m) = putTMVar m ()

acquireMutexIO :: MonadIO m => Mutex -> m ()
acquireMutexIO = liftSTM . releaseMutex

releaseMutexIO :: MonadIO m => Mutex -> m ()
releaseMutexIO = liftSTM . acquireMutex

runHeld :: MonadIO m => Mutex -> m a -> m a
runHeld m act = do
    acquireMutexIO m
    a <- act
    releaseMutexIO m
    return a

data Locked a = Locked Mutex (TVar a)

newWithLock :: MonadIO m => a -> m (Locked a)
newWithLock a = liftSTM $ Locked <$> newMutex <*> newTVar a

lock :: MonadIO m => Locked a -> m ()
lock (Locked m _) = acquireMutexIO m

unlock :: MonadIO m => Locked a -> m ()
unlock (Locked m _) = releaseMutexIO m

runLocked :: MonadIO m => Locked a -> (TVar a -> m b) -> m b
runLocked (Locked m t) f = do
    acquireMutexIO m
    b <- f t
    releaseMutexIO m
    return b

modifyLocked :: MonadIO m => Locked a -> (a -> a) -> m ()
modifyLocked l f = modifyLockedM l (return . f)
{-# INLINE modifyLocked #-}

modifyLockedM :: MonadIO m => Locked a -> (a -> m a) -> m ()
modifyLockedM l f = modifyLockedWithM l $ f >=> \a -> return ((), a)
{-# INLINE modifyLockedM #-}

modifyLockedWith :: MonadIO m => Locked a -> (a -> (b, a)) -> m b
modifyLockedWith l f = modifyLockedWithM l (return . f)
{-# INLINE modifyLockedWith #-}

modifyLockedWithM :: MonadIO m => Locked a -> (a -> m (b, a)) -> m b
modifyLockedWithM (Locked m t) f = do
    a <- liftSTM $ do
            acquireMutex m
            readTVar t
    (b, a') <- f a
    liftSTM $ do
        writeTVar t a'
        releaseMutex m
    return b

readLocked :: MonadIO m => Locked a -> m a
readLocked (Locked m t) = do
    a <- liftSTM $ do
            acquireMutex m
            readTVar t
    releaseMutexIO m
    return a
