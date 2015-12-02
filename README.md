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
  3. Give HasRoute instances for each route of your server, judiciously
     choosing prefixes.

And a client for the server obtains the data necessary for requests by
importing the individual routes, *and* the server type against which it shall
make requests.

See the [example](./Example.hs).
