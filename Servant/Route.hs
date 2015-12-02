{-|
Module      : Servant.Route
Description : Class for route aggregation and prefixing.
Copyright   : (c) Alexander Vieth, 2015
Licence     : BSD3
Maintainer  : aovieth@gmail.com
Stability   : experimental
Portability : non-portable (GHC only)
-}

{-# LANGUAGE AutoDeriveTypeable #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Servant.Route where

import GHC.TypeLits
import Servant
import Servant.Server
import Servant.Server.Internal.Enter
import Control.Monad.Trans.Either
import Network.Wai

-- | Show that a server (some type) has a route (some servant route type which
--   is not a :<|> alternative, i.e. one particular route).
--
--   An instance gives a possibly different type for the route, called the
--   FullRoute. This allows a route to be, for example, prefixed with one or
--   more static (non-capture) segments. Thus we can define particular routes
--   without including prefixes which indicate what they're all about, and only
--   throw in those prefixes when we bundle them with other routes.
--   Example: CRUD routes for blog posts can be defined like
--
--     @
--       type GetPost = Get '[JSON] Post
--       type PutPost = Capture "id" PostId :> ReqBody '[JSON] Post :> Put '[JSON] ()
--       type PostPost = ReqBody '[JSON] Post :> Post '[JSON] PostId
--       type DeletePost = Capture "id" PostId :> Delete '[JSON] ()
--     @
--
--   and they could be included with other routes not related to post by giving
--   HasRoute instances such that
--
--     @
--       RoutePrefix SomeServer GetPost = ["posts"]
--       RoutePrefix SomeServer PutPost = ["posts"]
--     @
--
--   and so on for the other routes.
--   This is useful because it allows Haskell clients who wish to interact
--   with a server to use the particular routes in order to obtain the form
--   of the request, and the particular server which has those routes to
--   obtain the full url with prefix.
--
--   Other members of this class indicate the monad in which the route is
--   run, complete with a natural transformation allowing us to use it in
--   a servant application.
class HasServer route => HasRoute server route where
    type RoutePrefix server route :: [Symbol]
    type RouteMonad server route :: * -> *
    serverRoute :: Proxy server -> Proxy route -> ServerT (FullRoute server route) (RouteMonad server route)
    routeMonadEnter :: Proxy server -> Proxy route -> (RouteMonad server route :~> EitherT ServantErr IO)


type family PrefixRoute (prefix :: [Symbol]) route where
    PrefixRoute '[] route = route
    PrefixRoute (p ': ps) route = p :> PrefixRoute ps route

type FullRoute server route = PrefixRoute (RoutePrefix server route) route

-- | Apply FullRoute to every route against a given server.
type family FullRoutes server routes where
    FullRoutes server (route :<|> routes) = FullRoute server route :<|> FullRoutes server routes
    FullRoutes server route = FullRoute server route

-- | Indicates that a server has all of these routes. This lifts
--   HasRoute up through the :<|> type.
class HasRoutes server routes where
    serverRoutes :: Proxy server -> Proxy routes -> Server (FullRoutes server routes)

instance {-# OVERLAPS #-}
    ( HasRoute server route
    , HasRoutes server routes
    , Enter
          (ServerT (FullRoute server route) (RouteMonad server route))
          (RouteMonad server route :~> EitherT ServantErr IO)
          (ServerT (FullRoute server route) (EitherT ServantErr IO))
    ) => HasRoutes server (route :<|> routes)
  where
    serverRoutes proxyServer proxyRoutes = thisServer :<|> thoseServers
      where
        thisServer = enter (routeMonadEnter proxyServer (Proxy :: Proxy route))
                           (serverRoute proxyServer (Proxy :: Proxy route))
        thoseServers = serverRoutes proxyServer (Proxy :: Proxy routes)

instance {-# OVERLAPS #-}
    ( HasRoute server route
    , FullRoutes server route ~ FullRoute server route
    , Enter
          (ServerT (FullRoutes server route) (RouteMonad server route))
          (RouteMonad server route :~> EitherT ServantErr IO)
          (ServerT (FullRoutes server route) (EitherT ServantErr IO))
    ) => HasRoutes server route
  where
    serverRoutes proxyServer proxyRoute = enter (routeMonadEnter proxyServer proxyRoute)
                                                (serverRoute proxyServer proxyRoute)

-- | Obtain a Wai Application for a given server and routes. This will run
--   on the FullRoutes for that pair, using HasRoute instances to determine
--   the appropriate servers and enter transformations for each route.
serveRoutes
    :: forall server routes .
       ( HasRoutes server routes
       , HasServer (FullRoutes server routes)
       )
    => Proxy server
    -> Proxy routes
    -> Application
serveRoutes proxyServer proxyRoutes = serve (Proxy :: Proxy (FullRoutes server routes))
                                            (serverRoutes proxyServer proxyRoutes)
