{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}
module Control.Monad.Trans.NSWebApp
    (
    -- * The NSWebAppT monad transformer
    NSWebAppT(..),
    runNSWebAppT
    ) where

import           Control.Applicative         (Alternative)
import           Control.Monad.Base          (MonadBase (..))
import           Control.Monad.Catch         (MonadCatch, MonadThrow)
import           Control.Monad.Error.Class   (MonadError(..))
import           Control.Monad.IO.Class      (MonadIO (..))
import           Control.Monad.Log           (Handler, LoggingT (..),
                                              MonadLog (..), runLoggingT)
import           Control.Monad.Reader        (MonadReader (..), ReaderT (..))
import           Control.Monad.Trans         (MonadTrans (..))
import           Control.Monad.Trans.Control (MonadBaseControl (..))
import           Control.Monad.Trans.Except  (ExceptT (..), runExceptT, throwE, catchE)


-- | A monad transformer that adds exceptions, read only state and logging to
-- other monads.
--
-- @NSWebAppT@ construct a monad parameterized over four things:
--
-- *e -- The exception type.
--
-- *r -- The read only state type.
--
-- *message -- Log message type.
--
-- *m -- The inner monad.
--
newtype NSWebAppT e r message m a = NSWebAppT
    { unNSWebAppT :: ExceptT e (ReaderT r (LoggingT message m)) a
    } deriving (Functor,Applicative,Monad,Alternative,MonadIO,MonadReader r,MonadThrow,MonadCatch)

instance Monad m => MonadLog message (NSWebAppT e r message m) where
    logMessage m =
        NSWebAppT
            (ExceptT
                 (ReaderT
                      (\_ ->
                            LoggingT
                                (ReaderT
                                     (\f ->
                                           (f m >>= return . Right))))))

instance MonadTrans (NSWebAppT e r message) where
  lift = NSWebAppT . lift . lift . lift

instance Monad m => MonadError e (NSWebAppT e r message m) where
    throwError = NSWebAppT . throwE
    catchError m h = NSWebAppT (catchE (unNSWebAppT m) (unNSWebAppT . h))

deriving instance MonadBase b m => MonadBase b (NSWebAppT e r message m)

instance MonadBaseControl IO m => MonadBaseControl IO (NSWebAppT e r message m) where
    type StM (NSWebAppT e r message m) a = StM (ReaderT r (LoggingT message m)) (Either e a)
    liftBaseWith runInBase =
        NSWebAppT
            (ExceptT
                 (liftBaseWith
                      (\runInExcept ->
                            runInBase
                                (\(NSWebAppT (ExceptT m)) ->
                                      runInExcept m) >>=
                            return . Right)))
    restoreM st = NSWebAppT (ExceptT (restoreM st))

-- | Run execution within monad stack
runNSWebAppT :: NSWebAppT e r message m a
             -> r
             -> Handler m message
             -> m (Either e a)
runNSWebAppT app r handler =
    runLoggingT (runReaderT (runExceptT (unNSWebAppT app)) r) handler
