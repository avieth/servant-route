{-# LANGUAGE AutoDeriveTypeable #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}

import Data.Functor.Identity
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T
import Servant
import Servant.Route
import Network.Wai
import Network.Wai.Handler.Warp

type Stuff = T.Text
type GetStuff = Get '[JSON] Stuff
type PostStuff = ReqBody '[PlainText] Stuff :> Post '[JSON] ()

type Junk = T.Text
type GetJunk = Get '[PlainText] Junk

data TheServer
type TheRoutes = GetStuff :<|> PostStuff :<|> GetJunk

instance HasRoute TheServer GetStuff where
    type RoutePrefix TheServer GetStuff = '["stuff"]
    serverRoute _ _ = pure (T.pack "Stuff")
    type RouteMonad TheServer GetStuff = Identity
    routeMonadEnter _ _ = Nat (pure . runIdentity)

instance HasRoute TheServer PostStuff where
    type RoutePrefix TheServer PostStuff = '["stuff"]
    serverRoute _ _ x = print x >> pure ()
    type RouteMonad TheServer PostStuff = IO
    routeMonadEnter _ _ = Nat liftIO

instance HasRoute TheServer GetJunk where
    type RoutePrefix TheServer GetJunk = '["junk"]
    serverRoute _ _ = T.pack <$> getLine
    type RouteMonad TheServer GetJunk = IO
    routeMonadEnter _ _ = Nat liftIO

application :: Application
application = serveRoutes (Proxy :: Proxy TheServer) (Proxy :: Proxy TheRoutes)

main = run 8082 application
