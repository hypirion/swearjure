---
layout: page
title: About Swearjure
image:
  feature: abstract-8.jpg
  credit: dargadgetz
  creditlink: http://www.dargadgetz.com/ios-7-abstract-wallpaper-pack-for-iphone-5-and-ipod-touch-retina/
modified: 2015-04-01
---

Swearjure is a turing complete subset of Clojure that only uses
non-alphanumerics. 

### History

[Gary Fredericks](https://github.com/gfredericks) and
[Tim McCormack](https://github.com/timmc) were the first to question and discuss
how expressive Clojure would be without alphanumerics, in the
[#clojure](http://clojure-log.n01se.net/date/2011-04-06.html#19:04) channel on
Freenode. The name Swearjure was coined in IRC as well, and became a way of
referring to using Clojure without alphanumerics around November 2012.

Since then, there has been ongoing work on improving the usability of Swearjure.
Turing completeness was in fact
[proven](http://hypirion.com/musings/swearjure-is-turing-complete) late January
2015, by Alex Engelberg and Jean Niklas L'orange independently of each other.
The turing completeness capabilities was accidentally brought in by Gary
Fredericks through [CLJ-1121](http://dev.clojure.org/jira/browse/CLJ-1121).

### Implementation

The Swearjure implementation was designed to explore the functionality of the
Clojure reader, which has curious edge cases. Some of them can be abused in
Swearjure to gain additional power. For instance, the syntax quote reader can be
abused to create hash maps of arbitrary length:

{% highlight clojure %}
(defn my-hash-map [& values]
  `{~@values ~@()})
{% endhighlight %}

And quoted arguments to function literals yield gensyms:

{% highlight clojure %}
user=> (#(identity '%) :foo)
p1__771#
{% endhighlight %}


After long waiting times to verify correctness, the author decided to make a
fully functional Swearjure REPL to lower feedback time. From there on, the
project quickly grew out of control, and is now a more or less fully usable
Clojure implementation[^1].

[^1]: Without alphanumeric characters in input expressions.
