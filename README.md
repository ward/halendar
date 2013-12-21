# Halendar - The Haskell Calendar

## What

This is a (simplistic) calendar for the Functional Programming course at the
Vrije Universiteit Brussel.

## Requirements

* Haskell
* Cabal
* Snap Framework (installed through cabal)
* SQLite 3

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

## Useful Reading

* [Snap: A Haskell Web Framework](http://snapframework.com/)
* [Snap.Types](http://hackage.haskell.org/package/snap-core-0.1.2/docs/Snap-Types.html)
* [Snap.Snaplet.SqliteSimple](http://hackage.haskell.org/package/snaplet-sqlite-simple-0.4.5/docs/Snap-Snaplet-SqliteSimple.html)
* [snaplet-sqlite-simple example](https://github.com/nurpax/snaplet-sqlite-simple/tree/master/example)
* [sqlite-simple](https://github.com/nurpax/sqlite-simple)
* [Snap.Snaplet.Auth](http://hackage.haskell.org/package/snap-0.6.0.2/docs/Snap-Snaplet-Auth.html)
