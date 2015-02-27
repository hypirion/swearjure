# Swearjure Tutorial

Hello, and welcome to the Swearjure tutorial! This will give you basic knowledge
on how to make working Swearjure programs. You should have some familiarity with
basic Clojure, but you don't have to be an expert. It would probably help to
know how syntax-quoting works, especially unquote and unquote-splicing.

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

The most essential type of value in Swearjure is an integer. Let's make some:

```clojure
swj> 10
(line 1, column 2):
Alphanumeric characters are not allowed
swj> 0
(line 1, column 2):
Alphanumeric characters are not allowed
```

Oh, right. In Swearjure, you're not allowed to use any alphanumeric characters
whatsoever. Of course, this means that we can't create integers by just writing
them down. Instead, we have to use arithmetic operators `+`, `-`, `*` and `/`.
But how can we use them if we don't have any numbers to use them on? The answer
lies within `+` and `*`:

```clojure
swj> (+)
0
swj> (*)
1
```

Clojure's `+` function returns 0 if given no arguments, and `*` returns 1. The
reasons has to do with
[monoids](https://gist.github.com/igstan/c3797e51aa0784a5d275), which is
interesting in and of itself, but not that relevant for Swearjure.

Now we can create any kind of integer we want by just adding together `(*)`s:

```clojure
swj> (+ (*) (*))
2
swj> (+ (*) (*) (*) (*))
4
```

We can also do more complex things, where we perform multiplication or
subtraction. Here, for instance, is (2 * 2) - 1:

```clojure
swj> (- (* (+ (*) (*)) (+ (*) (*))) (*))
3
```

We can also create ratios by using `/`:

```clojure
swj> (/ (*) (+ (*) (*) (*)))
1/3
swj> (/ (+ (*) (*)) (+ (*) (*) (*) (*)))
1/2 ; shortened version of 2/4
```

And that's essentially all the tricks you need to know on how to create numbers
in Swearjure. We will sometimes, to make life easier, write `` `1`` or `` `12``
instead of expressions that evaluate to those numbers later on. It would be
pretty hard to follow what's happening if we don't!

## Creating and Looking up Elements in Collections

Now we have numbers, so let's take a look at collections. Since all the
collections (list, vectors, maps and sets) have literal versions, we can just
use those:

```clojure
swj> '(+ - *)
(+ - *)
swj> [+ - *]
[#<core$+> #<core$-> #<core$*>]
swj> {:+ :-}
{:+ :-}
swj> #{+ :+ (*)}
#{#<core$+> :+ 1}
```

We don't frequently use lists in Swearjure, but if you want to use it in its
unquoted form, you have to use syntax-quoting:

```clojure
swj> `(~+ ~- ~*)
(#<core$+> #<core$-> #<core$*>)
swj> ;; or, which is a neat trick and might be easier to use:
swj> `(~@[+ - *])
(#<core$+> #<core$-> #<core$*>)
```

Oh, right, I forgot to mention: You can actually use alphanumerics in comments.
As they don't change the semantics of a program, you don't break Swearjure laws
by using them.

If you're a bit unfamiliar with syntax-quote, you can always quote the
expression to see what it actually evaluates to. It might be helpful if you're
confused by something:

```clojure
swj> '`(~+ ~- ~*)
(clojure.core/seq (clojure.core/concat (clojure.core/list +)
                                       (clojure.core/list -)
                                       (clojure.core/list *)))
swj> '`(~@[+ - *])
(clojure.core/seq (clojure.core/concat [+ - *]))
```

So, how do we look up elements in these collections? We can't use `get` or
`nth`, since they contain alphanumerics. So we can use the fact that sets,
vectors and maps are functions themselves!

```clojure
swj> ({+ -} +)
#<core$->
swj> (['+ '-] `0) ;; `0 == (+)
swj> (['+ '-] (+))
+
swj> (#{+ - *} *)
#<core$*>
```

If the element does not exist in the collection, we get back nil, or an error
for vectors:

```clojure
swj> ([] +)
Illegal argument: Key must be integer
swj> ([] (+))
Illegal argument: Index out of bounds for vector
swj> (#{} +)
nil
swj> ({} +)
nil
```

It's also possible to do lookup with symbols and keywords:

```clojure
swj> ('+ {'+ (*)})
1
swj> (:! #{:!})
:!
```

For maps, symbol and keywords, you can also provide a default argument if the
value does not exists in the collection:

```clojure
swj> ({} + :-)
:-
swj> (:- #{:!} :+)
:+
swj> ('++ {:++ :-} :!!)
:!!
```

... but we haven't looked at how to lookup elements in a list. How do we do
that? Since there is no non-alphanumeric function for that, we have to convert
lists to vectors first:

```clojure
swj> '(+ - *)
(+ - *)
swj> `[~@'(+ - *)]
[+ - *]
swj> (`[~@'(+ - *)] `1)
swj> (`[~@'(+ - *)] (*))
+
```

We will see some actual usage of this transformation when we come to basic
functions. However, you will rarely have to use this, as it is more common to
use vectors for more or less everthing.

## Conditionals

Conditionals are hard to do, for obvious reasons: We can't use `if`, which is
the basic primitive for running code conditionally. So we're in a bad spot: How
can we ensure that we only take one branch out of two?

Before we go into that problem, let us assume that taking both branches is
alright (that is, they will both terminate and neither will crash). If we, for
instance, have the Clojure expression

```clojure
(if conditional
  then
  else)
```

If we assume the expressions are legal in Swearjure, we only need to remove the
if. This not that difficult, when we remember that maps are functions. We can
then associate falsey values with the else expression, and the default value is
the then expression:

```clojure
({nil else
  false else} conditional
  then)
```

This is a bit clumsy though. Fortunately, most conditionals in Swearjure are
expressed by equal like this: `(= expected actual)`. In that case, the
expression

```clojure
(if (= expected actual)
  then
  else)
```

can be converted to the following:

```clojure
({expected then} actual else)
```

As an example, consider this inner part of a factorial function:

```clojure
(if (= n 0)
  1
  (* n (! (- n 1))))
```

This could be expressed as

```clojure
({n 1} 0 (* n (! (- n 1))))
```

*assuming* `(! (- n 1))` terminates. Of course, it's not that easy: If the inner
definition of factorial is defined like this, then it would never terminate!

### Non-Terminating Conditionals

The solution to expressions containing non-terminating computations is, of
course to defer the computation until it's actually needed. But how do we do
that without `if`? To see how we can do that, let's have a look at the following
Clojure expression:

```clojure
((if (= a b) + -)
 1 2)
```

In this expression, we get back 3 if a equals b, otherwise we get back -1.
However, and this is the important part: *The if statement returns a function*
*we then call*. Since we don't actually call both functions (only the one we
need), we sort-of have something that act as an if statement.

Let's go back to our original problem, and see if we can apply this there:

```clojure
(if (= n 0)
  1
  (* n (! (- n 1))))
```

The idea is to return a function inside the `if`, which we then immediately
call. This means that both functions must take the same amount of arguments. In
normal Clojure, we could do that rewrite like this:

```clojure
((if (= n 0) 
   (fn [x] 1)
   (fn [x] (* x (! (- x 1)))))
 n)
```

If you squint closely, you see that they are, in fact, the same. Understanding
this concept is important to be able to create good idiomatic Swearjure, so play
around with this idea in normal Clojure until you grasp it.

If we wanted to convert this particular part to Swearjure, we could do it like
this (assuming we have function literals available at hand):

```clojure
((if (= n 0)
   (fn [x] 1) ;; convert to {n 1} -- which always return 1
   (fn [x] (* x (! (- x 1))))) ;; convert to function literal
 n)

((if (= n 0) ;; change if by the rules above
   {n 1}
   #(* % (! (- % 1))))
 n)

(({n {n 1}} 0
   #(* % (! (- % 1))))
 n)

;; (Now change the n variable name to $ (something non-alphanumeric),
;;  and replace zeroes and ones)

(({$ {$ (*)}} (+)
   #(* % (! (- % (*)))))
 $)
```

Being able to do these transformations is the core essence of Swearjure, and it
is not always as easy as in this example.

## Basic Functions

The only way to create functions is by using the function literal `#()`. Let's
play around with this:

```clojure
swj> #()
#<user$eval1$fn__2>
swj> (#(+ % %) (*))
2
swj> (#(* % % %) `2)
swj> (#(* % % %) (+ (*) (*)))
8
swj> (#(%) `1)
swj> (#(%) (*))
Cast exception: Integer cannot be cast to IFn
swj> (#(%&) (*))
Cast exception: PersistentList cannot be cast to IFn
swj> (#(+ % (#(+ % %) %)) (*))
(line 1, column 11):
Nested #() are not allowed
```

We only have two literals available: We can use `%`, `%&`, or both. Of course,
we can't use `%1`, `%2`, `%3` and so forth, as they contain digits. However,
with proper use of `%&`, we can simulate them: Recall from the section about
collections that we can convert a list to a vector, and that we can lookup
elements in that vector:

```clojure
#(+ %1 %2) == #(+ % (`[~@%&] `0))

#(+ %1 %2 %3) == #(+ % (`[~@%&] `0) (`[~@%&] `1))
```

It looks pretty ugly, but is fully doable.

With that said, let us attempt to define some core functions in Swearjure
(`def`ing them to ease readability):

```clojure
(def swj-inc #(+ % `1))

(def swj-dec #(- % `1))

(def swj-identity #({} % %))
```

`identity` works, because whatever we get in, it will not be contained in an
empty map. Therefore the default value (`%`) will be returned.

```clojure
(def swj-list
   #([%&] `0))
;; Here we stick all the arguments in a vector, and immediately drag it out.

(def swj-vector
  #(`[[~@&%]] `0))
;; Combining swj-list and the lookup trick to convert the `%&` list to a vector.

(def swj-nth
  #(`[~@%] (`[~@%&] `0)))
;; Convert first argument to a vector, then call that vector with the second
;; argument.

(def swj-hash-map
  #(`[{~@%& ~@()}] `0))
```

The hash-map implementation is mighty weird. To have a better look at how it
works, let's quote it:

```clojure
swj> '`{~@%& ~@()}
(clojure.core/apply clojure.core/hash-map
  (clojure.core/seq (clojure.core/concat %& ())))
```

This makes sense, but is there any reason why we need the `~@()` there? Yes, if
we avoid it, we get an error saying "Map literal must contain an even number of
forms" in Clojure. In Swearjure, the parser just doesn't know what to do when
there aren't a pair of elements, and just breaks down:

```clojure
swj> {:+}
(line 1, column 4):
unexpected "}"
```

### Injecting Return Values

We can inject return values by using `->>` in Swearjure. They work like this:

```clojure
swj> (->> :+ #())
#<user$eval1$fn__2>
swj> ((->> :+ #()))
:+
```

Why does this work? It works because `#()` is expanded *before* the
macroexpansion – so in fact, the last expansion works like this:

```clojure
(->> :+ (fn* [] ()))
=> (fn* [] () :+)
```

Remember the nested functions problem? We can now circumvent that problem:

```clojure
swj> #(#())
(line 1, column 5):
Nested #() are not allowed
swj> ;; line below turns into (fn* [] () (fn* [] ()))
swj> (->> #() #())
#<user$eval13$fn__14>
swj> ((->> #() #()))
#<user$eval17$fn__18>
swj> (((->> #() #())))
()
```

However, we can't refer to values from the outer function in the inner function
(at least not yet). So, although we've found a way to insert functions into
other functions, we can't utilise closures just yet.

### Injecting Recursive Names/Parameters

We just saw how we can inject return values by using `->>`. What happens if we
use `->`? Well, it depends on what we put into the function. If we put a symbol,
we can refer to the function inside itself:

```clojure
swj> ;; line below turns into (fn* $ [] ([$](+)))
swj> ;; which is the same as  (fn* $ [] $)
swj> (-> $ #([$](+)))
#<user$eval23$$__24>
swj> ((-> $ #([$](+))))
#<user$eval25$$__26>
swj> ((((((((-> $ #([$](+))))))))))
#<user$eval27$$__28>
```

If we put a vector with symbols in it, we suddenly have a new arglist! Although
this doesn't seem to give us new powers, it's much more convenient when we want
to handle multiple args:

```clojure
swj> (-> [$ !] #(+ $ !))
#<user$eval29$$__30>
swj> ((-> [$ !] #(+ $ !)) (*) (+ (*) (*)))
3
swj> ;; We even have rest args by using &!
swj> ;; Line below is equivalent to (fn* [$ & !] [! $])
swj> ((-> [$ & !] #([[! $]] (+))) :+ :- :* :!)
[(:- :* :!) :+]
swj> ((-> [? ! $ _] #(* (+ ? !) (+ $ _))) `1 `2 `2 `2)
swj> ((-> [? ! $ _] #(* (+ ? !) (+ $ _))) (*) (+ (*) (*)) (+ (*) (*)) (+ (*) (*)))
12
```

## More Complex Functions

## I/O
