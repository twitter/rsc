# Reasonable Scala Compiler

Reasonable Scala compiler (also known as Rsc) is an experimental Scala compiler
focused on compilation speed. This project is developed by Eugene Burmako
and his team at Twitter.

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

* We came up with [a small subset of Scala](docs/language.md) that serves
as a tractable foundation for our performance work
* We implemented [a prototype typechecker](docs/compiler.md) that uses
[Scalameta](https://github.com/scalameta/scalameta)-inspired trees and
[Kentucky Mule](https://github.com/gkossakowski/kentuckymule)-inspired
architecture
* We benchmarked our typechecker, and it turned out to be
[significantly faster than the full Scalac typechecker](docs/performance.md)
* We will be carefully expanding the scope of Rsc
according to [our roadmap](../../milestones)

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
