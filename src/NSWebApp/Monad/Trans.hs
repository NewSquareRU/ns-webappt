{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE StandaloneDeriving         #-}
module NSWebApp.Monad.Trans
    ( NSWebAppT(..)
    , runNSWebAppT
    ) where

import           Control.Applicative       (Alternative)
import           Control.Monad.Error.Class (MonadError)
import           Control.Monad.Except      (ExceptT (..), runExceptT)
import           Control.Monad.IO.Class    (MonadIO)
import           Control.Monad.Log         (Handler, LoggingT (..),
                                            MonadLog (..), runLoggingT)
import           Control.Monad.Reader      (ReaderT (..))


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

runNSWebAppT :: NSWebAppT e r message m a
             -> r
             -> Handler m message
             -> m (Either e a)
runNSWebAppT app r handler =
    runLoggingT (runReaderT (runExceptT (unNSWebAppT app)) r) handler
