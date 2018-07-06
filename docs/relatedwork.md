<!-- Copyright (c) 2017-2018 Twitter, Inc. -->
<!-- Licensed under the Apache License, Version 2.0 (see LICENSE.md). -->

# Related work

Scala compilation technology is an active field of research.
At the time of writing, there are at least four actively developed Scala compilers.
In this document, we will compare Rsc with publicly available related work
(presented in chronological order).

## [Lightbend Scala](https://github.com/scala/scala)

Lightbend Scala is the official Scala compiler developed by Lightbend
and a big community of open-source contributors. It is a mature product
that enables hundreds of thousands of Scala programmers to get their job done,
and it's difficult to overestimate its importance to the community.

In comparison with Lightbend Scala, we are starting from a clean slate.
Nevertheless, we are definitely not declaring Lightbend Scala dead.
To the contrary, we view ourselves as complementary to Lightbend Scala and
hope that our results will create new opportunities for the official compiler.
We are planning to keep in contact with Lightbend to discuss our findings and
facilitate technology transfer.

## [Dotty](https://github.com/lampepfl/dotty)

Dotty is a rewrite of Lightbend Scala, scheduled to become Scala 3 sometime
after the Scala 2.15 release. It is developed by Martin Odersky,
his team at EPFL and a growing community of open-source contributors.

Similarly to Dotty, we are starting from a clean slate. Just like Martin Odersky
did about five years ago, we took an existing Scala parser and are writing
the typechecker from scratch. We draw a lot of inspiration from Martin's bold
undertaking.

Unlike Dotty, we are keeping our focus very narrow. Dotty improves on
Lightbend Scala in many ways, including language design, modern theoretical
foundations and innovations in compiler architecture. To the contrast, we are
a one-trick pony, willing to go to the extreme to reach our target in compilation
performance. We believe that Dotty and Rsc have complementary goals,
so we will be definitely staying in touch with Martin and his team to exchange ideas.

## [Typelevel Scala](https://github.com/typelevel/scala)

To quote its readme, Typelevel Scala is a conservative, collaborative
and binary compatible fork of Lightbend Scala. The intention is for it to
provide early access to bug fixes and enhancements of the Scala toolchain
which are of particular interest to projects which use the "Typelevel style" —
extensive use of type classes, generic programming and exploitation of the
distinctive features of Scala's type system such as higher-kinded, dependent
and singleton types.

Unlike Typelevel Scala, we are not a fork of Lightbend Scala. As discussed above,
we believe that we need the freedom to aggressively experiment in order to fully
explore ways to speed up Scala compilation.

## [Policy](https://github.com/paulp/policy)

Policy was an experimental fork of the Lightbend Scala compiler
started by Paul Phillips. At the moment, the GitHub page of Policy
is no longer available, and the fate of the project is unclear.

## [Kentucky Mule](https://github.com/gkossakowski/kentuckymule)

Kentucky Mule is an experimental typechecker for Scala developed by
Grzegorz Kossakowski in a very peculiar style. It only does a subset
of the work that Lightbend Scala is doing, but it does that an an impressive
speed of millions of lines of code per second. The last commit in its repository
is dated Dec 30, 2016, and it is unclear what will happen to it next.

Similarly to Kentucky Mule, our focus is compilation performance.
We believe that this is the most important problem that the Scala community
is facing. We used the findings of Kentucky Mule to build our typechecker,
so Greg's work was a direct inspiration for our project.

Unlike Kentucky Mule, we are aiming at building a full-blown compiler.
Thanks to the recent technological breakthroughs in
[Scalameta](http://scalameta.org/),
this is easier than ever, so we're willing to go the extra mile to gather more
performance-oriented insight for the community.

## [Hydra](https://triplequote.com/hydra)

Hydra is a closed-source Scala compiler developed by Triplequote Sàrl.
It is a drop-in replacement for the Lightbend Scala compiler that achieves
impressive speedups thanks to its use of proprietary technology.

Similarly to Hydra, our interests lie in compilation performance.
It is awesome to be part of the Scala community and work alongside
compiler hackers that push the boundaries of what is possible.

Unlike Hydra, we will be developed in the open. We believe that this will enable
more efficient cross-pollination of ideas and will bring dramatically faster
compilation times to everyone in the community.
