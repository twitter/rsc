Reasonable Scala Compiler
=========================

Reasonable Scala compiler (`rsc`) is an experimental Scala compiler
focused on compilation speed. This project is developed by Eugene Burmako
and his team at Twitter.

At Twitter, we have one of the biggest Scala codebases on the planet,
and compilation time is consistently among the top asks from our engineers.
With `rsc`, we seek to foster innovation in compilation performance,
openly prototyping performance-focused designs and making our findings
available to the Scala community at large.

Our project is inspired by the work of Martin Odersky and Grzegorz Kossakowski.
Martin showed us that in this day and age it is still possible to
[write a Scala compiler from scratch](https://github.com/lampepfl/dotty).
Greg showed us that
[compiling Scala can be blazingly fast](https://github.com/gkossakowski/kentuckymule).

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

What about existing compilers?
------------------------------

[relatedwork.md](relatedwork.md)

How do I get started?
---------------------

We are planning to open source Reasonable Scala compiler in the near future.
For now, star our project on GitHub and stay tuned for updates.
