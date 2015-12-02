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
-- Notice that we don't include any static parts of the route. This is
-- good! Where the route is placed on a server is a concern not of the route
-- itself, but the server and that route taken together.
type GetStuff = Get '[JSON] Stuff
type PostStuff = ReqBody '[PlainText] Stuff :> Post '[JSON] ()

type Junk = T.Text
type GetJunk = Get '[PlainText] Junk

data TheServer
type TheRoutes = GetStuff :<|> PostStuff :<|> GetJunk

-- The HasRoute instances are designed to be lightweight, so that we can
-- access them server- and client-side. Compare with the ImplementsRoute
-- class, which is probably only accessible server-side.
instance HasRoute TheServer GetStuff where
    -- Here we indicate that GetStuff goes under the "stuff" prefix.
    -- It shall be accessible at GET /stuff/
    type RoutePrefix TheServer GetStuff = '["stuff"]

instance HasRoute TheServer PostStuff where
    type RoutePrefix TheServer PostStuff = '["stuff"]

instance HasRoute TheServer GetJunk where
    type RoutePrefix TheServer GetJunk = '["junk"]

-- The ImplementsRoute instances are probably only accessible server-side, as it
-- contains or references the implementation of the handler.
instance ImplementsRoute TheServer GetStuff where
    type RouteMonad TheServer GetStuff = Identity
    serverRoute _ _ = pure (T.pack "Stuff")
    routeMonadEnter _ _ = Nat (pure . runIdentity)

instance ImplementsRoute TheServer PostStuff where
    type RouteMonad TheServer PostStuff = IO
    serverRoute _ _ x = print x >> pure ()
    routeMonadEnter _ _ = Nat liftIO

instance ImplementsRoute TheServer GetJunk where
    type RouteMonad TheServer GetJunk = IO
    serverRoute _ _ = T.pack <$> getLine
    routeMonadEnter _ _ = Nat liftIO

application :: Application
application = serveRoutes (Proxy :: Proxy TheServer) (Proxy :: Proxy TheRoutes)

main = run 8082 application
