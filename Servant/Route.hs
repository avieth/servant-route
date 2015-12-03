{-|
Module      : Servant.Route
Description : Types for route aggregation and prefixing.
Copyright   : (c) Alexander Vieth, 2015
Licence     : BSD3
Maintainer  : aovieth@gmail.com
Stability   : experimental
Portability : non-portable (GHC only)

This module gives types and classes to aid in the construction of servant
servers. The motivation comes from the sharing of servant route types between
client and server, and the idea that a resource is defined independently of
its place in a server's routing scheme. A resource, for example, which gets
a blog post, is the same resource regardless of the static route prefixes
assigned by a server. The client program should be written against the resource
and the server which hosts it, where the former determines the required HTTP
data (query parameters, capture variables, request body, etc.) and the
latter determines the full resource path (but not the host and port, of course).

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

module Servant.Route (

      IsResource(..)
    , Resource
    , FlattenRoutes
    , FullRoute

    ) where

import GHC.TypeLits
import Servant.API

-- | A resource is identified by a type, and determines a servant route type.
--   The type @t@ must be unique among the resource of each server which uses
--   it, but @ResrouceRoute t@ need not be, because it's possible that two
--   distinct resources have the same servant route type.
class IsResource (t :: *) where
    type ResourceRoute t :: *

-- | Indicate a resource. This is to be used in server types, along with
--   a type which is an instance of @IsResource@.
--   This type has an important role in the families @FullRouteK@ and
--   @FullRoute@. It allows @FullRoute@ to find a particular resource and give
--   its full (prefixed) route.
--   @FlattenRoutes@ will dispense with the Resource constructors, replacing
--   them with their @ResourceRoute@, to obtain a valid servant route (something
--   with a @ServerT@ type defined).
data Resource t

-- | Indicates that a given resource is not present in a routes type.
data ResourceNotPresent routes resource

-- | Indicates that a given resource is present in a routes type.
--   The third parameter gives the static prefix of the resource.
--
--   This should never escape to users of this module, as it is despensed with
--   by FullRoute through FullRouteFinish.
data ResourcePresent routes resource prefix

-- | Remove the Resource type constructors from the routes, giving a
--   servant routes type (something which has a ServerT type).
type family FlattenRoutes (routes :: *) :: * where
    FlattenRoutes (left :<|> right) = FlattenRoutes left :<|> FlattenRoutes right
    FlattenRoutes (piece :> pieces) = piece :> FlattenRoutes pieces
    FlattenRoutes (Resource t) = ResourceRoute t

-- | Compute the full route for a given route, among a set of routes, by use
--   of continuations. When using FullRouteK, the supplied value for the
--   first continuation should always be ResourcePresent routes resource '[],
--   and the second continuation should always be ResourceNotPresent
--   continuation is what you get in case the given route is not found in the
--   routes.
type family FullRouteK (routes :: *) (resource :: *) (kFound :: *) (kNotFound :: *) :: * where

    -- For an alternative, we search the left, then search the right in case
    -- nothing comes up.
    FullRouteK (left :<|> right) resource kFound kNotFound =
        FullRouteK left resource kFound (FullRouteK right resource kFound kNotFound)

    -- In this case we try the remaining pieces, tacking on this piece to
    -- the existing prefix.
    -- If nothing comes up, we use the not found continuation.
    FullRouteK (piece :> pieces) resource (ResourcePresent routes resource prefix) kNotFound =
        FullRouteK pieces resource (ResourcePresent routes resource (piece :> prefix)) kNotFound

    -- We have found a matching resource! Great. We're done.
    FullRouteK (Resource t) (Resource t) kFound kNotFound = kFound

    -- No match. The search resumes at the continuation.
    FullRouteK (Resource s) (Resource t) kFound kNotFound = kNotFound

type family FullRouteFinish (outcome :: *) :: * where
    FullRouteFinish (ResourceNotPresent routes resource) =
        ResourceNotPresent routes resource
    FullRouteFinish (ResourcePresent routes (Resource resource) reversePrefix) =
        PrefixRoute (Reverse reversePrefix) (ResourceRoute resource)

data EmptyPrefix

type family PrefixRoute prefix route where
    PrefixRoute EmptyPrefix route = route
    PrefixRoute (p :> ps) route = p :> PrefixRoute ps route

type family Snoc (prefix :: *) (p :: k) :: * where
    Snoc EmptyPrefix p = p :> EmptyPrefix
    Snoc (x :> xs) p = x :> Snoc xs p

type family Reverse list where
    Reverse EmptyPrefix = EmptyPrefix
    Reverse (x :> xs) = Snoc (Reverse xs) x

-- | Search the routes for a given route (under a Resource constructor). If
--   it is not present, the type is RouteNotPresent routes route.
type FullRoute routes resource =
    FullRouteFinish (FullRouteK routes (Resource resource)
                                       (ResourcePresent routes (Resource resource) EmptyPrefix)
                                       (ResourceNotPresent routes resource)
                    )
