{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Lib
import Control.Monad.Trans.Class 
import Control.Monad.Trans.Reader 
import Data.IORef
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as TL 
import System.Environment (getArgs) 
import Web.Scotty.Trans
import Control.Monad.IO.Class

{-newtype IORef a = IORef (STRef RealWorld a)-}

data Config = Config {
-- that's one, one click!
-- two...two clicks!
-- Three BEAUTIFUL clicks! ah ah ahhhh 
  counts :: IORef (M.Map Text Integer)
, prefix :: Text }
-- Stuff inside ScottyT is, except for things that escape
-- via IO, effectively read-only so we can't use StateT.
-- It would overcomplicate things to attempt to do so and
-- you should be using a proper database for production
-- applications.

{-data ScottyState e m = ScottyState { middlewares :: [Wai.Middleware]-}
                {-, routes :: [Middleware m]-}
                {-, handler :: ErrorHandler e m-}
                {-}-}

{-newtype State s a = State {runState :: s -> (s, a)}-}
{-newtype ScottyT e m a = ScottyT { runS :: State (ScottyState e m) a }-}
{-newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }-}
{-newtype ReaderT r m a = ReaderT { runReaderT :: r -> m a }-}
{-newtype StateT s m a = StateT { runStateT :: s -> m (a,s) }-}
{-newtype ExceptT e m a = ExceptT (m (Either e a))-}
{-newtype ActionT e m a = ActionT { runAM :: ExceptT (ActionError e) (ReaderT ActionEnv (StateT ScottyResponse m)) a }-}


type Scotty = ScottyT Text (ReaderT Config IO) 
type Handler = ActionT Text (ReaderT Config IO)

bumpBoomp :: Text
          -> M.Map Text Integer
          -> (M.Map Text Integer, Integer) 
bumpBoomp k m =  (M.insert k i m, i) where
    i = maybe 1 (+1) (M.lookup k m) 

app :: Scotty () 
app =
   get "/:key" $ do
      unprefixed  <- param "key"
      config <- lift ask
      let key' = mappend (prefix config) unprefixed
          ref = counts config
          map' = readIORef ref
      (newMap, newInteger) <- liftIO (bumpBoomp key' <$> map')
      liftIO $ writeIORef ref newMap
      html $ mconcat [ "<h1>Success! Count was: " , TL.pack $ show newInteger , "</h1>" ]

main :: IO () 
main = do
     [prefixArg] <- getArgs 
     counter <- newIORef M.empty 
     let config = Config counter (TL.pack prefixArg)
         runR r = runReaderT r config 
     scottyT 3000 runR app
