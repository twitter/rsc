# Rsc

Reasonable Scala compiler (also known as Rsc) is an experimental Scala compiler
focused on compilation speed. This project is developed by the Language Tools team at Twitter.

Rsc is not a fork, but a reimplementation of the Scala compiler.
We believe that a performance-oriented rewrite will provide a unique perspective
on compilation costs introduced by various Scala features and idioms -
something that is currently very hard to quantify in existing compilers.

With Rsc, our mission is to complement official compilers and assist with their
evolution through our experiments. We are aiming to discover actionable
insight into Scala compiler architecture and language design that will help
compiler developers at Lightbend and EPFL to optimize their compilers
for the benefit of the entire Scala community.

## Goals

* Dramatically improve Scala compilation performance
* Study compilation time overhead of various Scala features
* Identify a subset of Scala that can be compiled with reasonable speed
* Facilitate knowledge transfer to other Scala compilers

## Non-goals

* Full backward compatibility
(consider [Lightbend Scala](https://github.com/scala/scala) instead)
* New language features
(consider [Dotty](https://github.com/lampepfl/dotty) and
[Typelevel Scala](https://github.com/typelevel/scala) instead)
* Improvements to the type system
(consider [Dotty](https://github.com/lampepfl/dotty) and
[Typelevel Scala](https://github.com/typelevel/scala) instead)
* Runtime performance (will be addressed independently)

## Status

* We expanded the [supported subset of Scala](docs/language.md) and
are now using [Twitter Util](https://github.com/twitter/util)
in our experiments.
* We implemented support for loading dependencies based on
[the SemanticDB format](https://github.com/scalameta/scalameta/blob/master/semanticdb/semanticdb3/semanticdb3.md)
provided by [Scalameta](https://github.com/scalameta/scalameta).
* Our [prototype outliner](docs/compiler.md) can compute signatures of
public and protected definitions and save them in [the ScalaSignature format](https://github.com/scala/scala/blob/v2.11.12/src/reflect/scala/reflect/internal/pickling/PickleFormat.scala)
that enables interoperability with the Scala compiler.
* In the future, we will proceed with development according to
[our roadmap](docs/roadmap.md).

## Documentation

* [Getting started](docs/gettingstarted.md)
* [Language](docs/language.md)
* [Compiler](docs/compiler.md)
* [Performance](docs/performance.md)
* [Roadmap](docs/roadmap.md)
* [Contributing](docs/contributing.md)
* [Related work](docs/relatedwork.md)

## Credits

Our project is inspired by the work of Martin Odersky, Grzegorz Kossakowski
and Denys Shabalin. Martin inspiringly showed that in this day and age it is still
possible to [write a Scala compiler from scratch](https://github.com/lampepfl/dotty).
Greg unexpectedly demonstrated that
[compiling Scala can be blazingly fast](https://github.com/gkossakowski/kentuckymule).
Denys shockingly revealed that
[Scala apps can have instant startup time](https://github.com/scala-native/scala-native).
