<!-- Copyright (c) 2017-2018 Twitter, Inc. -->
<!-- Licensed under the Apache License, Version 2.0 (see LICENSE.md). -->

# Compiler

## Performance

Since compilation performance is the focus of the project, it is only fair
that we talk about it first before going into details of our compiler
architecture.

When planning our first milestone - a compiler that can typecheck nontrivial
programs - we were facing a dilemma. On the one hand, we saw the merit in
Greg's ["only the paranoid survive"](https://github.com/gkossakowski/kentuckymule#development-principles)
advice that advocates excessive attention to performance from day one.
On the other hand, we wanted to get something working as soon as possible
in order to make performance tuning more targeted, and that's at odds with
initially spending too much time on performance.

After careful consideration, we decided to make our architecture fundamentally
performance-friendly (e.g. no lazy completers, no tree transformations, etc),
but not get hung up on performance tuning (e.g. picking the right flavor of
a hashmap, figuring out the right representation for positions, etc)
before arriving at something that works end to end. We figured that since
we were going to start with [just a small subset of Scala](language.md),
even throwing away the entire compiler because it doesn't cut it
performance-wise isn't going to be too bad.
(And indeed, it wasn't too bad - in October, we rewrote almost everything,
although for a different reason).

## Trees

Scalac has notoriously desugared syntax trees. Starting very early in the
compilation pipeline, Scalac introduces a significant amount of non-syntactic
information into trees (for example, [this is how Scalac parses class definitions](https://github.com/scala/scala/blob/v2.12.4/src/reflect/scala/reflect/internal/TreeGen.scala#L355-L434)).
The situation becomes more and more intense as the compilation progresses.

Excessive desugaring is one of the main reasons why writing developer tools
for Scala is really hard. Both [Scalameta](https://github.com/scalameta/scalameta)
and [Dotty](https://github.com/lampepfl/dotty) have tried to improve the
situation by bringing syntax trees close to syntax, but none of these projects
went beyond parser trees.

In Rsc, we dial this up to eleven. We are building the entire compiler on top
of trees that are modelled very close to syntax. Concretely, we don't allow
information not present in source code to appear in parameters of our trees.
This rules out magic trees such as `EmptyTree`, synthetic member trees such
as class parameter accessors and the like.

With this initiative, we are trying to validate the hypothesis that
tool-friendly trees can be both convenient and fast to work with.
It is unclear whether we will succeed in this endeavor, but it would have been
a shame to miss this chance to experiment. We will update this section of
the document once we obtain usability experiences and benchmark results.

```scala
...

final case class DefnClass(
    mods: List[Mod],
    id: TptId,
    tparams: List[TypeParam],
    ctor: PrimaryCtor,
    inits: List[Init],
    stats: List[Stat])
    extends DefnTemplate

final case class DefnDef(
    mods: List[Mod],
    id: TermId,
    tparams: List[TypeParam],
    params: List[TermParam],
    ret: Tpt,
    rhs: Option[Term])
    extends Stat
    with Outline

...
```

If you're familiar with [Scalameta](https://github.com/scalameta/scalameta),
you'll find our trees similar. We aren't using Scalameta directly,
because we need full control over our data structures, but we're feeling that
Scalameta trees have proven themselves well in a number of practical
applications, so we decided to adopt many of their design decisions.

One similarity with Scalameta is never using raw `Tree` as the type of tree
fields. In the example above, class statements are typed as `List[Stat]`
like in Scalameta, not `List[Tree]` like in Scalac/Dotty. The return type of
a method is typed as `Tpt` (aka "type tree") similarly to Scalameta, not `Tree`
like in Scalac/Dotty. This design principle is not limited to just the trees
shown above, but permeates the entire tree design in Rsc.

One difference from Scalameta is the fanatic adherence to `sealed`. All base
traits in the `Tree` hierarchy (as well as all other data structures in the
compiler) are sealed, which means that we are forced to write exhaustive pattern
matches everywhere. This design principle synergizes extremely well with
strongly-typed trees.

We were somewhat concerned about enforcing such rigorous typing discipline.
Our experience with developing Scalac shows that it is occasionally convenient
to handle seemingly unrelated trees together or to sneak unexpected tree nodes
in unexpected locations, and both of these tricks are impossible in our design.
So far, we haven't run into any flexibility issues in Rsc, but we will wait
with the final judgement on this issue until we have enough experience with
the compiler.

## Parser

One of the choices for an Rsc parser was borrowing a first-party Scala parser
and modifying it to fit our trees. However, we found that both Scalac and Dotty
parsers are quite convoluted (in both Scala compilers in question, parsers are
sizeable files with couple thousands lines of code that involve tricky
desugarings), so we decided to look elsewhere.

Another choice was leveraging a third-party Scala parser,
e.g. [Scalaparse](https://github.com/lihaoyi/fastparse/tree/master/scalaparse)
by Haoyi Li or [scala-parser](https://github.com/paulp/scala-parser)
by Paul Phillips. We considered this opportunity, but eventually decided against
it. We felt that the requirements for a Scala compiler - precise positions,
careful handling of newline insertion and Scalac-compatible error reporting -
are too demanding for the state of the art of technologies underlying the
aforementioned third-party parsers.

Finally, we considered taking the parser from Scalameta and tweaking it to
produce Rsc trees. Unfortunately, the Scalameta parser is known to be
about an order of magnitude slower than its Scalac and Dotty counterparts,
so we didn't feel comfortable using it to build the foundation of
a performance-oriented compiler.

Ultimately, we decided to write our own parser inspired by Dotty source code.
One could view our effort as a refactoring for the Dotty parser that makes it
emit Rsc trees and strives to make the original code modular. Since our trees
are closer to syntax than Dotty trees are, we have been able to achieve
significant simplification of parser code.

```scala
final class Parser private (
    val settings: Settings,
    val reporter: Reporter,
    val input: Input)
    extends Bounds
    with Contexts
    with Defns
    with Groups
    with Helpers
    with Imports
    with Lits
    with Messages
    with Mods
    with Newlines
    with Params
    with Paths
    with Pats
    with Sources
    with Templates
    with Terms
    with Tpts
```

One big change that we made is trivialization of position handling.
A lot of code in both Scalac and Dotty parsers is dedicated to dealing with
the fact that trees don't always match program syntax. This gives rise to tree
envelopes, synthetic positions, position points and so on. We are not completely
sure, but our hypothesis is that in Rsc we can avoid this tricky business,
and so far we've been doing alright.

Another big change that we made is Scalameta-inspired scanning. Among Scala
compiler hackers, it is well-known that the scanner/parser interface is one
of the hairiest places in Scala compilers. We think that this complexity comes
from the fact that scanners in both Scalac and Dotty produce tokens that are
markedly desugared: spaces are dropped altogether, most comments are dropped
as well and newlines are postprocessed using a tricky algorithm. In Rsc,
we follow the spirit of Scalameta - representing source code exactly as it has
been written - and so far it had an overall simplifying effect on the compiler.

To tell the truth, we were anxious about bringing Scalameta principles to Rsc.
Scala folklore says that one of the reasons behind pervasive desugaring in
Scalac is optimizing for speed. Therefore, we were concerned that extra
abstractions - like whitespace tokens or Option-typed fields in trees -
may slow us down. At this point, [everything has been alright](performance.md),
but we will be definitely keeping an eye on this aspect of our architecture.

## Symbols

Following Dotty, we decided to separate symbols from their meanings.
Furthermore, following Scalameta, we decided to turn symbols into dumb tokens
that don't carry any data and don't define any methods. Given that we
significantly changed the original concept, we decided to rename
symbols to something else. Without further ado, meet uids.

```scala
type Uid = String
val NoUid: Uid = ""

private var counter = 0
def freshUid(): Uid = {
  counter += 1
  counter.toString
}
```

You can see that, unlike in trees, here we avoid options and use null objects
instead. If you're expecting us to back up this design decision with
benchmarks, then you're absolutely right, but we don't have the numbers just yet.
When we were deciding on options vs null objects in the typechecker, there was
no typechecker and, therefore, there was nothing to benchmark. Now we have
a working typechecker, so we'll be following up with numbers in the near future.

Meanings of uids are stored in symbol tables, with members represented
in `Symtab.scopes` and signatures represented in `Symtab.outlines`.
Looks like that it's all that's needed to encode the functionality
of symbols in Scalac and Dotty. (We are currently using the `HashMap` from the
JDK, but we will be benchmarking handwritten alternatives similar to those
found in Scalac, Dotty and Kentucky Mule.)

```scala
final class Symtab {
  val scopes: Map[Uid, Scope] = new HashMap[Uid, Scope]
  val outlines: Map[Uid, Outline] = new HashMap[Uid, Outline]
}
```

Scopes are collections of uids that support entering uids under simple ids
and looking up uids using simple ids. Following the language specification,
such a simple id can be either a `TermSid` or a `TypeSid`.

```scala
sealed abstract class Scope(val uid: Uid) {
  def enter(sid: Sid, uid: Uid): Uid
  def lookup(sid: Sid): Uid
  ...
}
```

Following [Kentucky Mule](https://github.com/gkossakowski/kentuckymule),
we gave up on lazy completers used in Scalac/Dotty and decided in favor
of eager completers. As a result, our scopes track their own status
(pending, blocked on someone's completion, failed or succeeded) and provide
dedicated functionality to perform lookups respecting that status.
Status-aware name resolution is quite tedious, but luckily we have been able
to limit its necessity to a very small part of the typechecker
(we will provide a detailed explanation below).

```scala
sealed abstract class Scope(val uid: Uid) {
  ...

  var status: Status = PendingStatus

  def resolve(sid: Sid): Resolution = {
    status match {
      case PendingStatus =>
        BlockedResolution(this)
      case BlockedStatus(_) =>
        BlockedResolution(this)
      case _: FailedStatus =>
        ErrorResolution
      case SucceededStatus =>
        unreachable(this)
    }
  }

  ...
}
```

Finally, outlines are regular trees such as `DefnClass`, `DefnDef` and others.
Unlike in Scalac/Dotty, we don't have dedicated data structures such as
`ClassInfoType` or `MethodType` to represent signatures. We store relevant
information in trees and then go directly to trees to figure out, say, parents
of a class or the return type of a method. (Future benchmarking may very well
show that this simplification leads to a performance degradation,
in which case we will have to decide whether this design decision is worth it.)

```scala
sealed trait Outline extends Tree {
  def id: Id
}
```

## Types

Since [our type system](language.md#types) is very rudimentary,
the associated data structures are very simple. We realize that this is
likely to blow up once we start extending the type system with additional
features from vanilla Scala.

```scala
sealed trait Type

final case object NoType extends Type

final case class SimpleType(uid: Uid, targs: List[SimpleType]) extends Type

final case class MethodType(
    tparams: List[Uid],
    params: List[Uid],
    ret: SimpleType)
    extends Type
```

Interestingly, Rsc settles a long-standing debate between us and Martin Odersky
(and, lately, Fengyun Liu). In Scalameta, we advocated for unification of
type trees and types, conjecturing that both concepts can be represented with
the same data structure. In Rsc, we saw with our own eyes that such design
doesn't work - a parser needs a data structure that reifies syntactic
peculiarities, whereas a typechecker needs a data structure that
canonicalizes those peculiarities. As a result, in the language model of Rsc,
we have both `Tpt` ("type tree") and `Type`.

## Typechecking

Typechecking programs in Scala can be split into two different steps
colloquially called "naming" and "typing" in Scalac and Dotty. Naming
involves discovering the definitions that constitute the program and computing
their signatures, while typing involves typechecking expressions such
as method bodies and constructor calls.

For example, in order to make sense of the program below, the typechecker: 1)
discovers class `A`, method `A.foo`, class `B`, method `B.c` and others
and computes their signatures, 2) goes through method bodies and typechecks
their parts, e.g. code like `b.c` using the knowledge obtained during
the first step.

<table>
  <tr>
    <td width="50%">
<pre><code>
class A {
  def foo: Foo = {
    val b: B = ...
    b.c
  }
}
</code></pre>
    </td>
    <td>
<pre><code>
class B {
  def c: C = {
    ...
  }
}

</code></pre>
    </td>
  </tr>
</table>

While it is clear that naming must happen before typing (otherwise, in the code
above, we won't be able to verify that `b` contains a method called `c`), it
is not clear how to order these compilation steps.

To demonstrate that it's a hard problem, let's analyze one obvious algorithm
that looks as follows:
1) Traverse all top-level definitions.
1) As the definitions are discovered, compute signatures for their members.
1) After the traversal is complete, proceed with typing.

This algorithm is pretty easy to explain and implement, but it also has
a fatal flaw - it doesn't take into the account that top-level definitions
can have legal circular dependencies. For example, if we add methods
`def A.b: B` and `def B.a: A` to our original example, then regardless
of the traversal order, the algorithm is going to produce an error.

<table>
  <tr>
    <td width="50%">
<pre><code>
class A {
  def foo: Foo = {
    val b: B = ...
    b.c
  }
  def b: B = { ... }
}
</code></pre>
    </td>
    <td>
<pre><code>
class B {
  def c: C = {
    ...
  }
  def a: A = { ... }
}

</code></pre>
    </td>
  </tr>
</table>

One refinement to the algorithm suggested above would be the following:
1) Traverse all definitions.
2) As the definitions are discovered, simply remember them and move on.
3) After the first traversal is complete, traverse the definitions again.
4) As the definitions are traversed, compute their signatures.
5) After the second traversal is complete, proceed with typing.

This algorithm can handle both code examples above, but it starts breaking
down as we take into account more and more Scala features. It turns out that
the second traversal is order-dependent, because signatures can depend on each
other because of features like imports, inheritance, return type inference,
etc etc.

<table>
  <tr>
    <td width="50%">
<pre><code>
import M._
class A {
  def foo: Foo = {
    val b: B = ...
    b.c
  }
  def b: B = { ... }
}
</code></pre>
    </td>
    <td>
<pre><code>
object M extends X {
  class B {
    def c: C = {
      ...
    }
    def a: A = { ... }
  }
}
</code></pre>
    </td>
  </tr>
</table>

## Typechecking in Scalac/Dotty

As you can see from the examples above, explicit scheduling of naming and typing
can get really hairy.

That's why both Scalac and Dotty sidestep the problem. During the first
traversal in the algorithm above, they assign definitions with so called
"completers" - thunks that compute signatures on demand. Afterwards,
they skip the second traversal altogether and jump straight to typing.

When typing requires a given signature (e.g. when processing `b.c`),
the compiler triggers its completer. If the completer depends on other
signatures (e.g. the completer of `B.c` depends on the signature of `X`),
then it triggers the completers of those signatures, etc. This way, instead
of trying to schedule the second traversal eagerly, Scalac and Dotty rely on
laziness to find out the correct schedule for computing signatures.

One complication with lazy completers is that we have to be cautious about
genuine cyclic reference errors. If there is a dependency cycle between
signatures (e.g. `class A extends B` and `class B extends A`), then lazy
completers will blow the stack. As a result, when starting completion for
a given definition, we must mark this definition as "in progress" and then
check for this mark before starting subsequent completions.

Apart from this complication, lazy completers work pretty well,
as demonstrated by Scalac and Dotty. Below you can find an excerpt from Scalac
that implements  `Symbol.info` - the method that computes signatures of
definitions. (The excerpt is significantly simplified to avoid exposing
implementation details of several other compiler subsystems.)

```scala
private var _info: Type = NoType

def info: Type = {
  if (!_info.isComplete) {
    val tp = _info
    if ((_flags & LOCKED) != 0L) {
      _info = ErrorType
      throw CyclicReference(this, tp)
    } else {
      _flags |= LOCKED
    }
    try {
      // The line below completes the signature
      // by assigning the computed type to _info.
      tp.complete(this)
    } finally {
      _flags &= ~LOCKED
    }
  }
  _info
}
```

That was the good news. The bad news is that completers significantly affect
compilation performance. First, they bring unpredictability to typechecking,
making it hard for humans to understand what's going on and build a reliable
mental model of performance. More importantly, they make parallelism very hard.

Let us elaborate on parallelism. Even though our explanations above may have
made it look like in Scala programs everything depends on everything, in reality
there is a significant degree of parallelism in typechecking. If we imagine
that an oracle has magically performed naming for us, then we can type all
methods completely in parallel.

Unfortunately, lazy completers prevent us from fully leveraging
this parallelism. If we naively run typing in parallel right after having set up
lazy completers, this will mean that multiple threads may attempt to run the
same completer in parallel. In turn, this means that only the first thread
will succeed, and the others will see the "in progress" mark on the associated
definition and will spuriously report cyclic reference errors.

We have discovered this problem several years ago while trying to make
runtime reflection in scala.reflect thread-safe. For more than a month,
we tried different strategies of fixing the spurious cyclic reference errors,
but eventually we gave up and hid completions behind a global lock. While this
ensured correctness, this also destroyed parallelism. Maybe there exists a
better solution, but we're unaware of it.

## Typechecking in Kentucky Mule

In a nutshell, [Kentucky Mule](https://github.com/gkossakowski/kentuckymule)
is a research namer that fixes the eager algorithm described above
and implements it for a nontrivial subset of Scala.
This is a breakthrough that makes it possible to implement a massively parallel
typer (but, unfortunately, the funding for Kentucky Mule had run out before
the author had a chance to start working on the typer).

Kentucky Mule keeps the first traversal that simply discovers the definitions,
but it modifies the second traversal to makes signature computations
interruptible. If a signature computation encounters an incomplete dependency,
it interrupts itself and adds itself to a special queue to be executed later.
Hopefully, by the time that computation is executed again, its dependencies
will have been resolved, otherwise the computation will be queued again.
This makes the order of the second traversal irrelevant.

Another key insight of Kentucky Mule is the lightweight interpretation of naming.
In Scalac and Dotty, naming does full typechecking of the computed signatures.
For example, `classSig` (a class completer) calls `typer.typedParentTypes`
to type the parent clauses of the class. In Kentucky Mule, naming only
resolves identifiers in signatures, but doesn't verify that the types in parent
clauses are well-typed, that there is a super constructor that can be called
with given arguments, etc. The resulting signatures are called outline types,
and, even though they are incomplete, they contain enough information to kick
off parallel typing. Kentucky Mule demonstrates that this lightweight naming
runs with incredible speed, which means that the compiler can get to parallel
typing in virtually no time.

The algorithm behind Kentucky Mule is described in detail in two blogposts:
[first](https://medium.com/@gkossakowski/can-scala-have-a-highly-parallel-typechecker-95cd7c146d20)
and [second](https://medium.com/@gkossakowski/kentucky-mule-limits-of-scala-typechecking-speed-6a44bd520a2f).
We recommend taking a look at these writeups, as they contain additional
insights that we don't have time to cover in this document.

## Typechecking in Rsc

Kentucky Mule is the direct inspiration for Rsc. Thanks to Kentucky Mule,
we knew for a fact that it should be possible to dramatically speed up Scala
compilation. This knowledge was invaluable for securing a future for Rsc.

Even though Kentucky Mule shows that parallel typing is possible, it doesn't
attempt to implement it. Neither it provides any clues as to how to write a
useful typer from scratch within a reasonable timeframe. This is where our
research begins, and this is what we'll present in the rest of the document.

First, we realized that there is a lightweight interpretation of typing.
Just like Kentucky Mule introduces a variation of namer that only resolves
names, but doesn't do bound checks, kind checks, etc, Rsc introduces a variation
of typer that only resolves names and performs desugarings, but doesn't do type
inference and the like. In a nutshell, we assume that underlying programs are
correct, making a lot of functionality of the typer unnecessary. We are planning
to implement that missing functionality in the future, but in the meanwhile we
can almost immediately deliver value to our users.

Secondly, we are making use of [Scalafix](https://github.com/scalacenter/scalafix).
Scalafix enables automated code rewrites, and that greatly simplifies migration
of existing Scala programs from vanilla Scala to Reasonable Scala.
As a result, we can start with [a small subset of the language](language.md)
(just like Kentucky Mule did), but be useful to a much bigger ecosystem.

Thirdly, we improved upon the peculiar style of Kentucky Mule completers.
These completers achieve greater flexibility (the ability to interrupt
themselves at critical points of execution) via a laborious encoding that
involves explicit yields. The author himself says "It's clear that for the
design based on interruptible completers to scale to the size of
a real compiler, a better way for expressing composition has to be found".

In our quest for this better way for expressing composition, we realized that
this extravagant style of completers is only necessary to complete a subset of
definitions - namely, imports and parent clauses (as well as definitions
with inferred return types, but only if we allow unrestricted imports, which
we don't have to). As a result, we can limit explicit yields to just two
moderately complicated cases, and develop the rest of the typechecker as usual.

Alternatively, you can view this as follows. Kentucky Mule introduces fast and
loose pre-typechecking (sequential computation of outlines during naming)
that enables parallel typing. Rsc introduces fast and loose pre-pre-typechecking
(sequential computation of import and parent outlines) that enables parallel
pre-typechecking.

## Codegen

We decided to postpone generation of executable code until we implement
typechecking for a sizable subset of the language. Therefore, there is nothing
to see in this section. Please come back later.

## Summary

At this point, Rsc is just a prototype, which means that:
A) it only supports [a small subset of Scala](language.md),
B) even on the supported subset of the language, its functionality is a subset
of functionality provided by Scalac. This section summarizes similarities
and differences between Rsc and Scalac.

<table>
  <th>
    <td>Rsc</td>
    <td>Scalac</td>
  </th>
  <tr>
    <td width="200px">Tokenize sources</td>
    <td width="250px">+ (parse)</td>
    <td width="250px">+ (parser)</td>
  </tr>
  <tr>
    <td>Parse sources</td>
    <td>+ (parse)</td>
    <td>+ (parser)</td>
  </tr>
  <tr>
    <td>Create toplevel symbols</td>
    <td>+ (schedule)</td>
    <td>+ (namer/packageobjects)</td>
  </tr>
  <tr>
    <td>Create global symbols</td>
    <td>+ (schedule)</td>
    <td>+ (typer)</td>
  </tr>
  <tr>
    <td>Create other symbols</td>
    <td>+ (typecheck)</td>
    <td>+ (typer)</td>
  </tr>
  <tr>
    <td>Compute signatures</td>
    <td>+ (scope/outline)</td>
    <td>+ (typer)</td>
  </tr>
  <tr>
    <td>Load classpath</td>
    <td>-</td>
    <td>+ (typer)</td>
  </tr>
  <tr>
    <td>Resolve names</td>
    <td>+ (scope/outline/typecheck)</td>
    <td>+ (typer)</td>
  </tr>
  <tr>
    <td>Infer implicits</td>
    <td>-</td>
    <td>+ (typer)</td>
  </tr>
  <tr>
    <td>Perform typechecks</td>
    <td>-</td>
    <td>+ (typer/post-typer phases)</td>
  </tr>
  <tr>
    <td>Generate bytecode</td>
    <td>-</td>
    <td>+ (post-typer phases)</td>
  </tr>
</table>
