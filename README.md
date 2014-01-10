# Halendar - The Haskell Calendar

## What

This is a (simplistic) calendar for the Functional Programming course at the
Vrije Universiteit Brussel.

## Requirements

* Haskell
* Cabal
* Snap Framework (installed through cabal)
* SQLite 3
* Library requirements are handled in the `*.cabal` file

## Installation

Halendar uses Haskell (duh) and as such we rely on `cabal`. So start off by
making sure you've got that installed. Next, use `cabal install snap` to install
the [Snap framework](http://snapframework.com/). Note that this can take a
while.

Now, while being in the project folder, issue `cabal install`. This will create
an executable in the following folder (on linux anyway): `$HOME/.cabal/bin/`.
Add that to your path with `PATH=$PATH:$HOME/.cabal/bin/` for ease of use.
Finally, issue `halendar` to start running Halendar.

Tables in the SQLite database will automatically be created when first run. If
using this in production, be sure to disable SQL query tracing in
`/snaplets/sqlite-simple/devel.cfg`.

## Understanding the code

This section is an overview of where to look for what in order to understand the
code. If you are used to the Snap framework, this will probably come more
easily. For those who aren't, I hope this explanation suffices.

### Directories

* `snaplets/heist/templates/` are all the template files. Heist is the template
  engine. These files will be used to generate pages to serve to the user.
  Beyond being able to include one another, it's also possible to expose bind
  points to the haskell code (luckily!).
* `snaplets/sqlite-auth/` has configuration options for the Auth snaplet.
* `snaplets/sqlite-simple/` has configuration options for the sqlite database.
* `src/` holds the actual haskell code, more details to follow further on.
* `static/` holds files that can be served directly. For example images, CSS, ...

### Time.hs

This file defines some functions that work on the `UTCTime` type from the
`Data.Time` module from the `time` library. The `repeat*` ones take a tuple of
two `UTCTime` and start tacking on the appropriate length, skipping entries that
don't exist in another month/year. For example 29 February 2012 in either start
of end of something repeating yearly would have as next result 29 February 2016
as there is no 29 February in 2013, 2014 and 2015. The `get*` ones are pretty
obvious.

### Db.hs

This one does three major things. First of all it defines the types we'll be
using: an `Event` type and a `User` type that is a sort of simplified version of
`AuthUser` from the `Auth` snaplet. Besides that it also provides functions to
create the necessary database table as well as checking if it is already there.
Finally there are functions that put an `Event` in the database as well as
getting it back out.

### Application.hs

This is pretty much some Snap configuring.

### Main.hs

This is where the magic happens! At least that's what I keep telling myself.

On the one hand there's a bit to set up Snap completely. This includes setting
up the routes from URLs to our handlers, setting up the snaplets that we are
using and defining the `main` function.

On the other hand there's a boatload of handler functions for the different
pages in the site as well as their helper functions. One of those helper
functions worthy of some special attention is `withLoggedInUser` which is a
wrapper function of sorts that

1. Checks if a user is logged in. If not, redirect to general "you need to log in" page
2. If logged in, simplify `AuthUser` to the `Db.User` we wrote, then pass that
   user on to the function which we are wrapping.

Now this wrapped function can be assured there is somoeone logged in as well as
knowing *who* it is.

Something else to also take note of is that all the handlers of pages with a
form are essentially two handlers in one. One of them triggers on a GET request
and shows the form, the other triggers on a POST request and handles the
submitted form.

## Useful Reading

* [Snap: A Haskell Web Framework](http://snapframework.com/)
* [Snap.Types](http://hackage.haskell.org/package/snap-core-0.1.2/docs/Snap-Types.html)
* [Snap.Snaplet.SqliteSimple](http://hackage.haskell.org/package/snaplet-sqlite-simple-0.4.5/docs/Snap-Snaplet-SqliteSimple.html)
* [snaplet-sqlite-simple example](https://github.com/nurpax/snaplet-sqlite-simple/tree/master/example)
* [sqlite-simple](https://github.com/nurpax/sqlite-simple)
* [Snap.Snaplet.Auth](http://hackage.haskell.org/package/snap-0.6.0.2/docs/Snap-Snaplet-Auth.html)
