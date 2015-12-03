# servant-route

## Motivation

The motivation for this small module comes from the construction of Haskell
clients for servant servers. Client-side, we would like to import the very
same route definitions used server-side, and use them to derive safe requests.
These should give us all the information we need in order to make the request,
besides the hostname and port of the server, and whether or not it's a secure
server. However, this is only true in case each route is composed of its
*full* path on the server. But we'd like routes to be as slim as possible.
There's no need, for instance, to define all user-related routes with the
prefix `"user" :>`. Instead, this prefix should be added to the routes when
the multi-route server is defined.

## Usage

With `servant-route`, a server is built following this process:

  1. Define new types for each resource, and give instances of `IsResource`
     which determine the servant route type. It's important to separate the
     servant route type from the resource type, as two semantically different
     resources *may* have the very same route type (remember, we're not
     concerned here with the place of the route in a server, only the essential
     things like capture variables, query params, request body, etc.).
  2. Define server types in the usual way, except that resources defined fom
     step 1 are included by giving `Resource t` where `t` is their unique type
     (the one which is an instance of `IsResource`).

Providing an implementation of the server is as usual, except that the
`Resource` constructors found in the server type must first be eliminated
via the type family `FlattenRoutes`. This gives a type which has a `ServerT`
whenever all of the `ResourceRoute` types from `IsResource` instances
also have `ServerT`. So instead of trying to serve your server type `myServer`,
you must instead serve `FlattenRoutes myServer`.

Working client-side, a server type (not flattened via `FlattenRoutes`) and a
resource type are used to come up with a url by running them through `FullRoute`. 
Whenever the resource is present, the result is the full route of that
resource on that server, from which a tool like `servant-xhr` can compute
the required parameters and resource part of a url (the part after the host and
port).

## An example

```Haskell
-- Datatypes used by our API, not directly related to routing.
data BlogPost
data Comment

-- GetBlogPost identifies a resource. Its ResourceRoute includes only the
-- information essential to the resource, and is not concerned with any static
-- route pieces which a server might add for organization purposes.
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
    type ResourceRoute PostComment = Capture "id" Int :> ReqBody '[JSON] Comment :> Post '[JSON] Int

-- A server for blog-post-related things only.
type PostServerV1 =
         Resource GetBlogPost
    :<|> Resource PostBlogPost

-- A second version for blog-post-related things, which adds the delete
-- resource.
type PostServerV2 =
         Resource GetBlogPost
    :<|> Resource PostBlogPost
    :<|> Resource DeleteBlogPost

-- A server for comment-related things only.
type CommentServerV1 =
         Resource GetComments
    :<|> Resource PostComment

-- Now the entire blog server, version 1. We add organizational prefixes
-- to distinguish post-related requests from comment-related requests.
type BlogServerV1 =
         ("post" :> PostServerV1)
    :<|> ("comment" :> CommentServerV1)

-- Again for version 2. We'll change the static parts just for fun.
type BlogServerV2 =
    "blog" :> (    ("posts" :> PostServerV2)
              :<|> ("comments" :> CommentServerV1)
              )
```

This example is taken even further in [Example.hs](./Example.hs).

With these definitions, we can use `FullRoute` with reference the a particular
server type:

```Haskell
:kind! FullRoute BlogServerV1 GetBlogPost
= "post" :> (Capture "id" Int :> Get '[JSON] BlogPost)

:kind! FullRoute BlogServerV1 PostComment
= "comment" :> (Capture "id" Int :> (ReqBody '[JSON] Comment :> Post '[JSON] ()))

:kind! FullRoute BlogServerV2 DeleteBlogPost
= "blog" :> ("posts" :> (Capture "id" Int :> Delete '[JSON] BlogPost))

:kind! FullRoute BlogServerV1 DeleteBlogPost
= ResourceNotPresent BlogServerV1 DeleteBlogPost
```
