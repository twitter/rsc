<!-- Copyright (c) 2017-2018 Twitter, Inc. -->
<!-- Licensed under the Apache License, Version 2.0 (see LICENSE.md). -->

# Language

At this point, Rsc supports just a small subset of Scala. In the future,
we plan to support more and more language features from vanilla Scala,
but currently our functionality is pretty modest.

The subset of Scala that we support is called Reasonable Scala,
and this document informally describes it. In the future, we may decide to
provide a formal specification for Reasonable Scala.

The decision to start with a subset of Scala not only allows us to get off the
ground fast, but also provides a tractable baseline for our performance work.
By starting small and adding functionality in a controllable manner, we will be
able to measure the impact of language features and idioms on compilation times.

Since not all Scala programs are compatible with Reasonable Scala,
a [Scalafix](https://github.com/scalacenter/scalafix) migration is likely to
to be required to use Rsc. However, all Reasonable Scala programs are compatible
with Scala, so codebases that have been migrated should be crosscompilable.
Details will become clearer down the line, but keep in mind that development of
Rsc is sponsored by a large Scala shop, so we take compatibility extremely seriously.

## First impressions

In its current form, Reasonable Scala looks and feels like vanilla Scala,
with additional boilerplate required to fill in for the missing language
features. Here's an excerpt from our example project called
[re2s](../examples/re2s).

```scala
package java.util.regex

import java.util.regex.CharClass._

/**
 * A "builder"-style helper class for manipulating character classes
 * represented as an array of pairs of runes [lo, hi], each denoting an
 * inclusive interval.
 *
 * All methods mutate the internal state and return {@code this}, allowing
 * operations to be chained.
 */
// inclusive ranges, pairs of [lo,hi].  r.length is even.
class CharClass(private var r: Array[Int]) {
  // prefix of |r| that is defined.  Even.
  private var len: Int = r.length

  // After a call to ensureCapacity(), |r.length| is at least |newLen|.
  private def ensureCapacity(_newLen: Int): Unit = {
    var newLen: Int = _newLen
    if (r.length < newLen) {
      // Expand by at least doubling, except when len == 0.
      if (newLen < len * 2) {
        newLen = len * 2
      }
      val r2: Array[Int] = new Array[Int](newLen)
      System.arraycopy(r, 0, r2, 0, len)
      r = r2
    }
  }

  ...
}
```

## Definitions

Reasonable Scala supports most of the ways to define program elements
supported by vanilla Scala (including packages, classes with their markedly
concise primary constructor syntax, traits and objects), with the exception of:
  * Inner and local classes, traits and objects.
  * Overloading (and, consequently, secondary constructors).
  * Multiple parameter lists (and, consequently, implicit parameters).
  * Several convenience features like package objects, pattern definitions,
  early definitions and others.

## Types

Reasonable Scala supports only a small subset of Scala's type system.
At this point, it's just generics and type aliases.

More advanced features, including higher-kinded types, existential types,
abstract type members, union and intersection types, structural types,
type lambdas, type projections, singleton types and path-dependent types are
left for future work.

## Expressions

Reasonable Scala enables programmers to get things done, but it doesn't support
many convenience features of vanilla Scala. Most notable omissions
include:
  * Inference (type inference, implicit inference, overload resolution).
  * Almost all desugarings and desugaring-based features (we only support
  `apply` and `update`, and the rest is future work).
  * Many pattern matching features (we only support a limited subset of
  expressions, type tests, wildcards and alternatives).
