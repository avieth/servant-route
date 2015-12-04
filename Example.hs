{-# LANGUAGE AutoDeriveTypeable #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}

import GHC.Generics
import GHC.TypeLits
import Control.Monad.Trans.Except
import Data.Aeson
import Servant.API
import Servant
import Servant.Route
import Network.Wai
import Network.Wai.Handler.Warp

-- An example of use. We have a server which supports two versions of an API,
-- each of which organizes its resources under route prefixes.

-- Some types for use in the resources, just so we can actually demonstrate
-- a working server.
data BlogPost = BlogPost Int
deriving instance Generic BlogPost
instance ToJSON BlogPost
instance FromJSON BlogPost

data Comment = Comment Int
deriving instance Generic Comment
instance ToJSON Comment
instance FromJSON Comment

-- Resource definitions. Notice how they lack organizational static parts.
-- GetBlogPost, for example, is not given a "posts" prefix, since that's
-- irrelevant to the resource.

data GetBlogPost
instance IsResource GetBlogPost where
    type ResourceRoute GetBlogPost = Capture "id" Int :> Get '[JSON] BlogPost

data PostBlogPost
instance IsResource PostBlogPost where
    type ResourceRoute PostBlogPost = ReqBody '[JSON] BlogPost :> Post '[JSON] Int

data DeleteBlogPost
instance IsResource DeleteBlogPost where
    type ResourceRoute DeleteBlogPost = Capture "id" Int :> Delete '[JSON] BlogPost

data GetComments
instance IsResource GetComments where
    type ResourceRoute GetComments = Capture "id" Int :> Get '[JSON] [Comment]

data PostComment
instance IsResource PostComment where
    type ResourceRoute PostComment = Capture "id" Int :> ReqBody '[JSON] Comment :> Post '[JSON] ()

-- Server definitions using resources.

type PostServerV1 =
         Resource GetBlogPost
    :<|> Resource PostBlogPost

type PostServerV2 =
         Resource GetBlogPost
    :<|> Resource PostBlogPost
    :<|> Resource DeleteBlogPost

type CommentServerV1 =
         Resource GetComments
    :<|> Resource PostComment

-- With post and comment servers in hand, we can create an aggregate server,
-- organizing these servers under "post" and "comment" respectively.

type BlogServerV1 =
         ("post" :> PostServerV1)
    :<|> ("comment" :> CommentServerV1)

-- For a version 2 server, let's change up the prefixes, just for fun.
type BlogServerV2 =
    "blog" :> (    ("posts" :> PostServerV2)
              :<|> ("comments" :> CommentServerV1)
              )

-- Finally, a multi-version blog server, which of course prefixes both
-- servers with the relevant version.
--
-- Note that some resources appear multiple times in this server type!
-- In fact, every one of them except for DeleteBlogPost appears twice, so
-- when you try FullRoute BlogServer GetBlogPost, for example, it resolves
-- the v2 prefix, *only* because v2 comes before v1 in the form of the type.
-- Reversing the order would favour v1, so all of the routes except for
-- DeleteBlogPost would resolve to the v1 route.
--
-- This should be OK. If a client program imports this server and tries to
-- resolve one of the resources which appear multiple times, it'll still work,
-- because those resources have the same type no matter where they appear, i.e.
-- the input and output is the same regardless of the version. It could be that
-- they have different server implementations, but that should not matter, since
-- they implement the very same resource. If there are observable differences,
-- it's because one of the implementations is doing something silly.
type BlogServer =
         ("v2" :> BlogServerV2)
    :<|> ("v1" :> BlogServerV1)

-- Here's how we put it all together. It's not so different from typical
-- servant usage; you just have to remember to FlattenRoutes when giving
-- the types.

-- These two types are the same, since FlattenRoutes just dispenses with
-- the Resource constructor.
--getBlogPostServer :: Server (FlattenRoutes (Resource GetBlogPost))
getBlogPostServer :: Int -> ExceptT ServantErr IO BlogPost
getBlogPostServer = pure . BlogPost

postBlogPostServer :: BlogPost -> ExceptT ServantErr IO Int
postBlogPostServer (BlogPost i) = pure i

deleteBlogPostServer :: Int -> ExceptT ServantErr IO BlogPost
deleteBlogPostServer = pure . BlogPost

getCommentsServer :: Int -> ExceptT ServantErr IO [Comment]
getCommentsServer = const (pure [])

postCommentServer :: Int -> Comment -> ExceptT ServantErr IO ()
postCommentServer = const (const (pure ()))

postServerV1 :: Server (FlattenRoutes PostServerV1)
postServerV1 = getBlogPostServer :<|> postBlogPostServer

postServerV2 :: Server (FlattenRoutes PostServerV2)
postServerV2 = getBlogPostServer :<|> postBlogPostServer :<|> deleteBlogPostServer

commentServerV1 :: Server (FlattenRoutes CommentServerV1)
commentServerV1 = getCommentsServer :<|> postCommentServer

blogServerV1 :: Server (FlattenRoutes BlogServerV1)
blogServerV1 = postServerV1 :<|> commentServerV1

blogServerV2 :: Server (FlattenRoutes BlogServerV2)
blogServerV2 = postServerV2 :<|> commentServerV1

blogServer :: Server (FlattenRoutes BlogServer)
blogServer = blogServerV2 :<|> blogServerV1

application :: Application
application = serve (Proxy :: Proxy (FlattenRoutes BlogServer)) blogServer

main = run 8082 application
