Reasonable Scala Compiler
=========================

Reasonable Scala compiler (`rsc`) is an experimental Scala compiler
focused on compilation speed. This project is developed by Eugene Burmako
and his team at Twitter.

`rsc` is not a fork, but a clean-room reimplementation of a Scala compiler.
We believe that a performance-oriented rewrite will provide a unique perspective
on compilation costs introduced by various Scala features and idioms -
something that is currently very hard to quantify in existing compilers.

With `rsc`, our mission is to complement official compilers and assist with their
evolution through our experiments. We are aiming to discover actionable
insight into Scala compiler architecture and language design that will help
compiler developers at Lightbend and EPFL to optimize their compilers
for the benefit of the entire Scala community. Check out
[relatedwork.md](relatedwork.md) to learn more about how we compare
with other compilers and compiler technology research in Scala.

Goals
=====

* Dramatically improve Scala compilation performance
* Study compilation time overhead of various Scala features
* Identify a subset of Scala that can be compiled with reasonable speed
* Facilitate knowledge transfer to other Scala compilers

Non-goals
=========

* Full backward compatibility
(consider [Lightbend Scala](https://github.com/scala/scala) instead)
* New language features
(consider [Dotty](https://github.com/lampepfl/dotty) and
[Typelevel Scala](https://github.com/typelevel/scala) instead)
* Improvements to the type system
(consider [Dotty](https://github.com/lampepfl/dotty) and
[Typelevel Scala](https://github.com/typelevel/scala) instead)
* Runtime performance (will be addressed independently)

Credits
=====

Our project is inspired by the work of Martin Odersky and Grzegorz Kossakowski.
Martin showed us that in this day and age it is still possible to
[write a Scala compiler from scratch](https://github.com/lampepfl/dotty).
Greg showed us that
[compiling Scala can be blazingly fast](https://github.com/gkossakowski/kentuckymule).

FAQ
===

What is the speedup?
--------------------

We believe that it possible to achieve dramatic compilation speedups (5-10x)
for typical Scala codebases, and we are currently well on track to
realizing this vision.

How can this be possible?
-------------------------

* Our typechecker is based on the insights of
  [the Kentucky Mule project](https://github.com/gkossakowski/kentuckymule)
  developed by Grzegorz Kossakowski who has convincingly demonstrated
  that typechecking Scala can be embarrassingly parallel.
* Our compilation pipeline consists of only 4 passes over syntax trees in
  comparison with 20+ passes in Lightbend/Typelevel Scala and 15+ passes in Dotty.
* Furthermore, we believe that restricting our compiler to support just
  a subset of Scala will enable further compilation speedups. We call this
  subset "Reasonable Scala" and will dedicate focused effort to identifying it.

What language features will be supported?
-----------------------------------------

It is too early to say this definitively. We are planning to start small
with a trivial subset of Scala and then gradually add features,
carefully measuring their impact on compilation performance.
Ideally, we would like to make it possible for Scala programmers to easily reason
about compilation performance cost of various Scala features and idioms.

What about existing codebases?
------------------------------

Not all Scala programs will be compatible with Reasonable Scala, so a
[Scalafix migration](https://github.com/scalacenter/scalafix) may be required
to use `rsc`. However, all Reasonable Scala programs will be compatible with
Scala, so codebases that have been migrated will be crosscompilable.
Details will become clearer down the line, but keep in mind that we are
a large Scala shop, so we take compatibility extremely seriously.


How do I learn more?
--------------------

We don't have any further official documentation, but there has been
a great discussion of our announcement on Reddit and Hacker News.
Check out the links below if you're interested in details:
  * [/r/scala](https://www.reddit.com/r/scala/comments/6ubuix/twitter_announces_reasonable_scala_compiler_an/)
  * [/r/programming](https://www.reddit.com/r/programming/comments/6ubuk0/twitter_announces_reasonable_scala_compiler_an/)
  * [Hacker News](https://news.ycombinator.com/item?id=15043316)

How do I get started?
---------------------

We are planning to open source Reasonable Scala compiler in the near future.
For now, star our project on GitHub and stay tuned for updates.
