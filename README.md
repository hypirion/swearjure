# Swearjure

It's Swearjure! A turing complete subset of Clojure, which only allows you to
work with non-alphanumeric characters.

Other notable differences from Clojure include:

- No mutability
- Strict (no laziness)
- Interpreted, not compiled
- Implicit tail recursion
- Fast startup time

## Usage

Build it by using the [cabal](https://www.haskell.org/cabal/) build tool for
Haskell, like so:

```bash
git clone https://github.com/hypirion/swearjure.git
cd swearjure
cabal sandbox init
cabal install
cp .cabal-sandbox/bin/swearjure swearjure
# and we're ready to go!
./swearjure
```

## Tutorial

Read the [tutorial][] on how to actually make usable Swearjure programs.

[tutorial]: https://github.com/hypirion/swearjure/blob/master/doc/tutorial.md

## Copying

Free use of this software is granted under the terms of the GNU Lesser General
Public License (LGPL). For details see the files COPYING and COPYING.LESSER
included with the source distribution. All copyrights are owned by their
respective authors.
