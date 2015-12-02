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

-- | States that a server (some type) has a route (some servant route type which
--   is not a :<|> alternative, i.e. one particular route).
--
--   An instance ultimately determines a possibly different type for the route,
--   called the FullRoute, via the RoutePrefix, which is a list of symbols.
--   This allows a route to be prefixed with one or more static (non-capture)
--   segments. Thus we can define particular routes without including prefixes
--   which indicate what they're all about, and only throw in those prefixes
--   when we bundle them with other routes.
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
--     @
--
--   and so on for the other routes.
--   This is useful because it allows Haskell clients who wish to interact
--   with a server to use the particular routes in order to obtain the form
--   of the request, and the particular server which has those routes to
--   obtain the full url with prefix.
class HasServer route => HasRoute server route where
    type RoutePrefix server route :: [Symbol]

class ServesRoutes server where
    type ServesRoutesDatum server :: *

-- | States that a server actually handles a route.
class 
    ( ServesRoutes server
    , HasRoute server route
    ) => ImplementsRoute server route
  where
    type RouteMonad server route :: * -> *
    type RouteMonad server route = EitherT ServantErr IO
    routeMonadEnter
        :: Proxy server
        -> Proxy route
        -> ServesRoutesDatum server
        -> (RouteMonad server route :~> EitherT ServantErr IO)
    serverRoute
        :: Proxy server
        -> Proxy route
        -> ServerT (FullRoute server route) (RouteMonad server route)

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
class HasRoutes server routes
instance {-# OVERLAPS #-}
    ( HasRoute server route
    , HasRoutes server routes
    ) => HasRoutes server (route :<|> routes)
instance {-# OVERLAPS #-}
    ( HasRoute server route
    ) => HasRoutes server route

-- | Indicates that a server implements all of these routes. This lifts
--   ImplementsRoute up through the :<|> type.
class
    ( ServesRoutes server
    , HasRoutes server routes 
    ) => ImplementsRoutes server routes
  where
    serverRoutes
        :: Proxy server
        -> Proxy routes
        -> ServesRoutesDatum server
        -> Server (FullRoutes server routes)

instance {-# OVERLAPS #-}
    ( ImplementsRoute server route
    , ImplementsRoutes server routes
    , Enter
          (ServerT (FullRoute server route) (RouteMonad server route))
          (RouteMonad server route :~> EitherT ServantErr IO)
          (ServerT (FullRoute server route) (EitherT ServantErr IO))
    ) => ImplementsRoutes server (route :<|> routes)
  where
    serverRoutes proxyServer proxyRoutes datum = thisServer :<|> thoseServers
      where
        thisServer = enter (routeMonadEnter proxyServer (Proxy :: Proxy route) datum)
                           (serverRoute proxyServer (Proxy :: Proxy route))
        thoseServers = serverRoutes proxyServer (Proxy :: Proxy routes) datum

instance {-# OVERLAPS #-}
    ( ImplementsRoute server route
    , FullRoutes server route ~ FullRoute server route
    , Enter
          (ServerT (FullRoutes server route) (RouteMonad server route))
          (RouteMonad server route :~> EitherT ServantErr IO)
          (ServerT (FullRoutes server route) (EitherT ServantErr IO))
    ) => ImplementsRoutes server route
  where
    serverRoutes proxyServer proxyRoute datum = enter (routeMonadEnter proxyServer proxyRoute datum)
                                                      (serverRoute proxyServer proxyRoute)

-- | Obtain a Wai Application for a given server and routes. This will run
--   on the FullRoutes for that pair, using HasRoute instances to determine
--   the appropriate servers and enter transformations for each route.
serveRoutes
    :: forall server routes .
       ( ImplementsRoutes server routes
       , HasServer (FullRoutes server routes)
       )
    => Proxy server
    -> Proxy routes
    -> ServesRoutesDatum server
    -> Application
serveRoutes proxyServer proxyRoutes datum = serve (Proxy :: Proxy (FullRoutes server routes))
                                                  (serverRoutes proxyServer proxyRoutes datum)
