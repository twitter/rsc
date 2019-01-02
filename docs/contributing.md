<!-- Copyright (c) 2017-2019 Twitter, Inc. -->
<!-- Licensed under the Apache License, Version 2.0 (see LICENSE.md). -->

# Contributing

  * Our project has an ambitious goal of speeding up a 15-year old compiler
  by an order of magnitude. This sounds very hard, but it's actually even harder
  than it sounds.
  * It is well known that performance improvements obtained in months of
  careful design and hard work can end up disappearing in weeks due to
  unclear reasons.
  * This means that in Rsc we have to apply unprecedented rigor, or otherwise
  risk failing the project. As a result, we decided to establish strict
  development policies to ensure the atmosphere of attention to detail.
  * In our policies, we are standing on the shoulders of giants.
  Most of the points as well as significant chunks of text below were borrowed
  from contribution guidelines of the Scala compiler, Scala.js and Scala Native.

## Contribution style

  * All contributions to Rsc go through GitHub pull requests that should be
  approved by at least one member of the Rsc team before being merged into
  the distribution.
  * Before starting a significant chunk of work - be if a new feature or
  a bugfix - consult the Rsc team via the issue tracker. Reviews take time,
  so developments that are not aligned with our roadmap may end up stuck in
  the review queue for a long time. Please submit an issue if you'd like to
  have something added to the roadmap.
  * We realize that contributions are hard work, but the stakes are very high.
  Therefore, it is possible that your pull request will require significant
  changes or even a rewrite to be accepted in Rsc. However, even a rejected or
  reverted pull request is valuable. It helps explore the solution space,
  so that everyone knows what works and what doesn't.
  * Some of our other projects, e.g. Scalameta, are friendly to experiments.
  For example, SemanticDB, one of the flagship features of Scalameta, started
  as a partially working hack with little to no tests that was gradually refined
  through a long series of pull requests. We recognize the benefit of rapid
  prototyping that this approach enables, but we cannot afford that in Rsc,
  since at any given moment we need to be able to confidently reason about the
  architecture of the compiler.
  * To sum it up, an ideal contribution is a fully finished bugfix or feature
  that follows the policies outlined in this document, has thorough tests and
  has been discussed with the Rsc team in advance.

## Coding style

  * Rsc has a strict policy regarding coding style. We believe that in the long
  run this will help us to maintain a clean and consistent codebase.
  This strategy has worked well for Scala.js, and we intend to follow it too.
  * All contributions should be formatted with `./bin/scalafmt`.

## Git style

  * Branches
    * You should always perform your work in its own Git branch.
    * The branch should be called after an issue in our issue tracker,
    e.g. `42` for a branch that addresses the issue 42, to clearly
    correlate the proposed changes with an existing design discussion.
    If your changes don't have an associated issue, you should create one.
    If your changes address multiple issues, you should pick a descriptive
    branch name that encompasses all those issues.
    * Pull requests should be issued from a branch other than master,
    or they won't be considered for review.
  * Commits
    * The first line of the commit message should be a descriptive sentence
    about what the commit is doing, written using the imperative style,
    e.g., "Fix typo", and should not exceed 70 characters. It should be possible
    to fully understand what the commit does by reading just this single line.
    * The first line can be enough if the commit is a small change. Otherwise,
    following the single line description should be a blank line followed by
    details of the commit, in the form of free text, or bulleted list.
    * If your work consists of multiple independent changes, you should split
    them into multiple commits - one commit per change, each accompanied by
    a detailed commit message as described above.
  * History
    * Pull request branches should always be rebased on top of master. Therefore,
    it's strongly recommended to not perform any merges on branches that you are
    planning to use for pull requests.
    * It's alright to add new commits to pull request branches to accommodate
    changes that were suggested by the reviewers.
    * It's not alright to rewrite history on pull request branches during review.
    We want to keep as much historical information as possible in order
    to assist code archaeology in the future.
