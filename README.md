# Halendar - The Haskell Calendar

## What

This is a (simplistic) calendar for the Functional Programming course at the
Vrije Universiteit Brussel.

## Requirements

* Haskell
* Cabal
* SQLite 3
* Library requirements are handled in the `*.cabal` file

## Installation

Halendar uses Haskell (duh) and as such we rely on `cabal`. So start off by
making sure you've got that installed.

Now, while being in the project folder, issue `cabal install`. This will start
downloading and compiling a *big* amount of packages. Feel free to do
[something](http://xkcd.com/303/) else while you wait. It will also create an
executable in the following folder (on linux anyway): `$HOME/.cabal/bin/`. Add
that to your path with `PATH=$PATH:$HOME/.cabal/bin/`. Finally, issue `halendar`
to start running Halendar.

Caveat: from my (limited) experience, `cabal` is pretty horrible with properly
managing all the dependencies of different projects. If you don't care about
what packages you already had installed and you are having issues with running
`cabal install`, see if removing `~/.cabal/` and `~/.ghc/` before trying it
helps you any further.

Tables in the SQLite database will automatically be created when first run. If
using this in production (why are you doing this?!), be sure to disable SQL
query tracing in `/snaplets/sqlite-simple/devel.cfg`.

## Understanding the code

This section is an overview of where to look for what in order to understand the
code. Some working knowledge of Snap and Heist is required and outside the scope
of this document.

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
