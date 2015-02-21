# Extra tricks in Swearjure

## Randomness

It's not evident how you could get randomness in Swearjure. In fact, it doesn't
seem possible to do this if you look at the host implementations Clojure and
ClojureScript. However, Swearjure is a completely different beast and can
generate random numbers and values.

If you've ever played around with Clojure, you know that hash maps and hash sets
do not impose any ordering on their values.

```clj
user=> #{1 2 3 4 5}
#{1 4 3 2 5}
user=> (seq (set (range 2 20)))
(7 4 15 13 6 17 3 12 2 19 11 9 5 14 16 10 18 8)
```

The question that might arise is whether it is legal to return different values
on different calls to seq or not. According to the [officials][clj-1302], *"keys
order == vals order == seq order"*. This means, of course, that any call to
`seq` should return the same sequence from the same map or set. But does that
rule apply on any sets/maps that are equal? Let's try it out!

[clj-1302]: http://dev.clojure.org/jira/browse/CLJ-1302 "CLJ-1302"

```clj
user=> (def a (shuffle (range 1000)))
#'user/a
user=> (def b (shuffle (range 1000)))
#'user/b
user=> (= a b)
false
user=> (= (seq a) (seq b))
false
user=> [(first a) (first b)]
[229 745]
```

So, although the sets are equal, there is no constraint that says if `(= a b)`
then `(= (seq a) (seq b))`. This is what this Swearjure implementation utilises
beneath the covers to get randomness: The ordering of any set and map is
randomized on their creation.

```clj
swj> #{(+) (*)}
#{1 0}
swj> #{(+) (*)}
#{1 0}
swj> #{(+) (*)}
#{1 0}
swj> #{(+) (*)}
#{0 1}
swj> #{(+) (*)}
#{0 1}
```

We might therefore be tempted to create a random generator by putting 0 and 1 in
a set, converting the set to a vector, then pick the first element of that
vector:

```clj
swj> (#(`[~@#{(+) (*)}](+)))
1
swj> (#(`[~@#{(+) (*)}](+)))
0
swj> (#(`[~@#{(+) (*)}](+)))
0
swj> (#(`[~@#{(+) (*)}](+)))
1
```

It looks right, but is actually not correct. To see why, let's have a look at
what the syntax-quoted part actually returns:

```clj
swj> '`[~@#{(+) (*)}]
(clojure.core/apply clojure.core/vector
  (clojure.core/seq (clojure.core/concat #{(+) (*)})))
```

Notice that the set itself, `#{(+) (*)}`, is created during read time, not as a
function call. Therefore, if we try to call this function multiple times, we
will always get the same answer back:

```clj
swj> (#([[(%) (%) (%) (%) (%) (%)]](+))
      #(`[~@#{(+) (*)}](+)))
[0 0 0 0 0 0] ;; or [1 1 1 1 1 1]
```

The trick is to ensure that the set itself is created every time a function is
called. We can do this by backquoting the vector itself:

```clj
swj> '`#{}
(clojure.core/apply clojure.core/hash-set
  (clojure.core/seq (clojure.core/concat)))

swj> '`#{~(+) ~(*)}
(clojure.core/apply clojure.core/hash-set
  (clojure.core/seq (clojure.core/concat
                      (clojure.core/list (+))
                      (clojure.core/list (*)))))

swj> `#{~(+) ~(*)}
#{1 0}
```

As such, an actual random number generator function can be defined as follows:

```clj
swj> #(`[~@`#{~(+) ~(*)}](+))
#<user$eval5$fn__6>
swj> (#([[(%) (%) (%) (%) (%) (%)]](+))
      #(`[~@`#{~(+) ~(*)}](+)))
[0 1 0 0 1 0] ;; Or some other random sequence of values
```

Note that this (sadly?) doesn't work in Clojure/ClojureScript, because sets and
maps are deterministically created. As mentioned, there is no "law" that states
this has to be true in a Clojure implementation.
