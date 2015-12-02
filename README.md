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

  1. Define individual routes as parsimoniously as possible.
  2. Define a type to represent your server, probably one with 0 constructors.
  3. Give a ServesRoutes instance for your server type, which indicates what
     extra data the server needs in order to work.
  4. Give HasRoute instances for each route of your server, judiciously
     choosing prefixes. These can be imported client-side and server-side.
  5. Give ImplementsRoute instances for each route of your server. These
     can be imported server-side (they won't be needed client-side).

A client for the server obtains the data necessary for requests by
importing the individual routes, the server type against which it shall
make requests, and all of the relevant `HasRoute` instances. Thus the
intermediary interface between client and server consists of those three
things:

  - Route types
  - Server type
  - `HasRoute` instances

Whereas the other players are relevant to server-side implementation only:

  - `ServesRoutes` instance
  - `ImplementRoute` instances

See the [example](./Example.hs).
