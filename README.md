# Halendar - The Haskell Calendar

## What

This is a (simplistic) calendar for the Functional Programming course at the
Vrije Universiteit Brussel.

## Installation

Halendar uses Haskell (duh) and as such we rely on `cabal`. So start off by
making sure you've got that installed. Next, use `cabal install snap` to install
the [Snap framework](http://snapframework.com/). Note that this can take a
while.

Now, while being in the project folder, issue `cabal install`. This will create
an executable in the following folder (on linux anyway): `$HOME/.cabal/bin/`.
Add that to your path with `PATH=$PATH:$HOME/.cabal/bin/` for ease of use.
Finally, issue `halendar` to start running Halendar.
