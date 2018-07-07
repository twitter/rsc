<!-- Copyright (c) 2017-2018 Twitter, Inc. -->
<!-- Licensed under the Apache License, Version 2.0 (see LICENSE.md). -->

# Language

At this point, Rsc supports a subset of Scala. It is enough to produce
signatures for automatically rewritten core of
[Twitter Util](https://github.com/twitter/util).

The biggest limitation is the lack of type inference for result types of
vals, vars and defs, as well as type arguments for super constructor calls.
We don't plan to address this limitation in the near future. Instead, we
rely on [Scalafix](https://github.com/scalacenter/scalafix) to automatically
rewrite Scala sources to be compatible with Rsc.

There are [other limitations](https://github.com/twitter/rsc/issues?q=is%3Aopen+is%3Aissue+label%3ALanguage),
and we will be working on addressing them according to [our roadmap](docs/roadmap.md).
