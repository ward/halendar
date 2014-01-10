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
`Data.Time` module from the `time` library. The `repeat*` ones takes a tuple of
two `UTCTime` and start tacking on the appropriate length, skipping entries that
don't exist in another month/year. For example 29 February 2012 in either start
of end of something repeating yearly would have as next result 29 February 2016
as there is no 29 February in 2013, 2014 and 2015. The `get*` ones are pretty
obvious.

## Useful Reading

* [Snap: A Haskell Web Framework](http://snapframework.com/)
* [Snap.Types](http://hackage.haskell.org/package/snap-core-0.1.2/docs/Snap-Types.html)
* [Snap.Snaplet.SqliteSimple](http://hackage.haskell.org/package/snaplet-sqlite-simple-0.4.5/docs/Snap-Snaplet-SqliteSimple.html)
* [snaplet-sqlite-simple example](https://github.com/nurpax/snaplet-sqlite-simple/tree/master/example)
* [sqlite-simple](https://github.com/nurpax/sqlite-simple)
* [Snap.Snaplet.Auth](http://hackage.haskell.org/package/snap-0.6.0.2/docs/Snap-Snaplet-Auth.html)
