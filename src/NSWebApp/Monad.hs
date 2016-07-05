
module NSWebApp.Monad
    ( NSWebAppIO
    ) where


import           NSWebApp.Monad.Trans (NSWebAppT (..))

type NSWebAppIO e r message a = NSWebAppT e r message IO a

