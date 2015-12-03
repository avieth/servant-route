{-# LANGUAGE AutoDeriveTypeable #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}

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

-- Some types for use in the routes, with unimplemented JSON instances just so
-- we can use them in a server.
data BlogPost
instance ToJSON BlogPost where
    toJSON = undefined
instance FromJSON BlogPost where
    parseJSON = undefined

data Comment
instance ToJSON Comment where
    toJSON = undefined
instance FromJSON Comment where
    parseJSON = undefined

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
type BlogServer =
         ("v1" :> BlogServerV1)
    :<|> ("v2" :> BlogServerV2)

-- Here's how we put it all together. It's not so different from typical
-- servant usage; you just have to remember to FlattenRoutes when giving
-- the types.

-- These two types are the same, since FlattenRoutes just dispenses with
-- the Resource constructor.
--getBlogPostServer :: Server (FlattenRoutes (Resource GetBlogPost))
getBlogPostServer :: Int -> ExceptT ServantErr IO BlogPost
getBlogPostServer = undefined

postBlogPostServer :: BlogPost -> ExceptT ServantErr IO Int
postBlogPostServer = undefined

deleteBlogPostServer :: Int -> ExceptT ServantErr IO BlogPost
deleteBlogPostServer = undefined

getCommentsServer :: Int -> ExceptT ServantErr IO [Comment]
getCommentsServer = undefined

postCommentServer :: Int -> Comment -> ExceptT ServantErr IO ()
postCommentServer = undefined

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
blogServer = blogServerV1 :<|> blogServerV2

application :: Application
application = serve (Proxy :: Proxy (FlattenRoutes BlogServer)) blogServer

main = run 8082 application
