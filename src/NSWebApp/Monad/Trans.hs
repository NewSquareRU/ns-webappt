{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE StandaloneDeriving         #-}
module NSWebApp.Monad.Trans
    (
    -- * The NSWebAppT monad transformer
    NSWebAppT(..),
    runNSWebAppT
    ) where

import           Control.Applicative       (Alternative)
import           Control.Monad.Error.Class (MonadError)
import           Control.Monad.Except      (ExceptT (..), runExceptT)
import           Control.Monad.IO.Class    (MonadIO)
import           Control.Monad.Log         (Handler, LoggingT (..),
                                            MonadLog (..), runLoggingT)
import           Control.Monad.Reader      (ReaderT (..))


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
    } deriving (Functor,Applicative,Monad,Alternative,MonadIO,MonadError e)

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


-- | Run execution within monad stack
runNSWebAppT :: NSWebAppT e r message m a
             -> r
             -> Handler m message
             -> m (Either e a)
runNSWebAppT app r handler =
    runLoggingT (runReaderT (runExceptT (unNSWebAppT app)) r) handler
