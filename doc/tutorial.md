# Swearjure Tutorial

Hello, and welcome to the Swearjure tutorial! This will give you basic knowledge
on how to make working Swearjure programs. You should have some familiarity with
basic Clojure, but you don't have to be an expert.

## Installing

Installation is currently done by building from source. As Swearjure is
implemented in Haskell, you'll have to get the project manager
[Cabal](https://www.haskell.org/cabal/) available on your computer. When you
have successfully installed Cabal, you can issue the following commands to get
Swearjure up and running:

```bash
git clone https://github.com/hypirion/swearjure.git
cd swearjure
cabal sandbox init
cabal install
cp .cabal-sandbox/bin/swearjure swearjure
./swearjure
```

If you want to put Swearjure on your `$PATH`, you can just copy `swearjure`
wherever to some place on your path. For example, if you have `~/bin` on your
path, you can just cp it like so:

```bash
cp swearjure ~/bin/
```

(You might have to reopen your shell to have make it available, as everything on
`$PATH` is usually cached)

### Usage

You can use Swearjure in three different ways. The first one is just the basic
repl:

```bash
./swearjure
swj>
```

This works as any other REPL, and accepts all legal Swearjure expressions.

You can also pipe in expressions from stdin, and they will evaluate in order.
Note though, that Swearjure will only start evaluating the expressions when
stdin doesn't have more to pipe in. (This behaviour might change in the future).
If the last expression returns nil, it is not printed out.

```bash
$ echo '[] (+ (*) (*))' | ./swearjure
2
$ echo '[(*) (+)] ({* +} -)' | ./swearjure
# (Nothing is printed)
$
```

Finally, Swearjure might take an input file to evaluate. This behaves exactly as
if the input file was piped in from stdin:

```bash
$ ./swearjure examples/hello-world.clj
Hello World!
# Which is equivalent to:
$ cat examples/hello-world.clj | ./swearjure
Hello World!
```

If you wonder what version of Swearjure you're using, you can always call it
with the `-h` or `--help` flag:

```bash
./swearjure -h
Swearjure, version (+).(*).(+)-SNAPSHOT (aka 0.1.0-SNAPSHOT)
```

## Numbers

## Looking up values

## Conditionals

## Basic Functions

### Gensyms

### Injecting Return Values

### Injecting Recursive Names/Parameters

## More Complex Functions

## I/O
