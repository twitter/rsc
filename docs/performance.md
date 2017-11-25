<!-- Copyright (c) 2017 Twitter, Inc. -->
<!-- Licensed under the Apache License, Version 2.0 (see LICENSE.md). -->

# Performance

Our research goal is to achieve dramatic compilation speedups (5-10x)
for typical Scala codebases, and we are currently well on track to reaching
this goal.

## Hardware

All benchmarks have been run on a desktop computer with an Intel Core
i7-4770 CPU (4 physical cores with support for Hyper-Threading, totaling
8 logical threads, 3.40 GHz base frequency, 3.90 GHz maximum Turbo Boost
frequency, 4x32+32KB L1 cache, 4x256KB L2 cache, 8MB L3 cache),
4x8GB 1600 MHz DDR RAM and a Crucial M500 240GB SSD.

For benchmarking, we locked CPU frequency to 3.40 GHz, disabling Turbo Boost
and dynamic frequency scaling features of the CPU. We also disabled
Hyper-Threading, leaving the CPU at 4 physical cores and 4 logical threads.

## Software

In our benchmarks, we used Debian GNU/Linux 9.2 (Linux kernel 4.9.0-4-amd64)
and Oracle Java 8 JDK build 1.8.0_151-b12 to run the current version of Rsc,
Scalac 2.11.11, Scalac 2.12.4 and Javac 1.8.0_151.

Our benchmarks run different fragments of compilation pipelines of
different compilers on two comparable codebases:
  * [re2j](../examples/re2j/src/main/java/java/util/regex), an implementation
    of linear time regular expression matching in Java (11740 loc).
  * [re2s](../examples/re2s/src/main/scala/java/util/regex), a port of re2j
    to Scala [performed in Scala Native](https://github.com/scala-native/scala-native/pull/894).
    For Rsc, re2s is accompanied by [Stdlib.scala](../stdlib/src/main/scala/Stdlib.scala),
    a source file that declares stubs for referenced definitions from
    scala-library and the JDK (11028 loc + 182 loc = 11210 loc).

We have run 11 families of benchmarks:
  * RscNativeSchedule: run the output of rscNative/nativeLink on re2s
    and Stdlib.scala with -Ystop-after:schedule.
  * RscSchedule: run the output of rscJVM/compile on re2s and Stdlib.scala
    with -Ystop-after:schedule.
  * ScalacNamer211: run Scalac 2.11.11 on re2s
    with -d tempdir -usejavacp -Ystop-after:namer.
  * ScalacNamer212: run Scalac 2.12.4 on re2s
    with -d tempdir -usejavacp -Ystop-after:namer.
  * RscNativeTypecheck: run the output of rscNative/nativeLink on re2s
    and Stdlib.scala with -Ystop-after:typecheck.
  * RscTypecheck: run the output of rscJVM/compile on re2s and Stdlib.scala
    with -Ystop-after:typecheck.
  * ScalacTyper211: run Scalac 2.11.11 on re2s
    with -d tempdir -usejavacp -Ystop-after:typer.
  * ScalacTyper212: run Scalac 2.12.4 on re2s
    with -d tempdir -usejavacp -Ystop-after:typer.
  * ScalacCompile211: run Scalac 2.11.11 on re2s
    with -d tempdir -usejavacp.
  * ScalacCompile212: run Scalac 2.12.4 on re2s
    with -d tempdir -usejavacp.
  * JavacCompile: run Javac 1.8.0_151 on re2j
    with -d tempdir.

Every family of benchmarks was run in two different modes: cold (performance
of a single run without warm-up) and hot (performance of a steady state achieved
by doing hundreds or even thousands of runs).

To benchmark native applications, we used
[our own microbenchmark harness](../bench/rsc/jvm/src/main/scala/rsc/bench/RscNativeTypecheck.scala):
  * Cold: compute a CLI invocation, run it using java.lang.ProcessBuilder,
    repeat 100 times, report mean execution time.
  * Hot: compute a CLI invocation that runs a benchmark 100 times,
    run it using java.lang.ProcessBuilder.

To benchmark JVM applications, we used JMH 1.19 that was run in sbt 0.13.16
via sbt-jmh 0.2.25 (flags for Scalac benchmarks as
well as JMH configurations for all benchmarks were inspired by
[scala/compiler-benchmark](https://github.com/scala/compiler-benchmark)):
  * Cold: run a benchmark using bench/jmh:run with @BenchmarkMode(Array(SingleShotTime))
    and @Fork(value = 128, jvmArgs = Array("-Xms2G", "-Xmx2G")).
  * Hot: run a benchmark using bench/jmh:run with @BenchmarkMode(Array(SampleTime)),
    @Warmup(iterations = 10, time = 10, timeUnit = TimeUnit.SECONDS),
    @Measurement(iterations = 10, time = 10, timeUnit = TimeUnit.SECONDS) and
    @Fork(value = 3, jvmArgs = Array("-Xms2G", "-Xmx2G")).

## Disclaimer

At this point, Rsc only implements a subset of functionality provided by the
Scala compiler. This means that the benchmark results provided below must
be interpreted with utmost care. Concretely:
  * Performance numbers may significantly deteriorate as we will be
    implementing more and more functionality of the Scala compiler.
    For example, adding support for classpath loading or implicit search
    is very likely to slow down our compiler by a significant factor.
  * Direct comparisons of Rsc and Scalac performance numbers should take
    into account similarities and differences in provided functionality.
    Consult [the summary in the "Compiler" document](compiler.md#summary)
    for more information.

## Results

To reproduce, run `sbt bench` (this will take a while).

<table>
  <th>
    <td>Cold</td>
    <td>Hot</td>
  </th>
  <tr>
    <td width="208px">RscNativeSchedule</td>
    <td width="208px">84.909 ms</td>
    <td width="208px">51.713 ms</td>
  </tr>
  <tr>
    <td>RscSchedule</td>
    <td>344.909 ± 0.771 ms</td>
    <td>10.945 ± 0.008 ms</td>
  </tr>
  <tr>
    <td>ScalacNamer211</td>
    <td>1179.715 ± 2.818 ms</td>
    <td>62.111 ± 0.099 ms</td>
  </tr>
  <tr>
    <td>ScalacNamer212</td>
    <td>1642.299 ± 2.927 ms</td>
    <td>27.683 ± 0.029 ms</td>
  </tr>
</table>

<table>
  <th>
    <td>Cold</td>
    <td>Hot</td>
  </th>
  <tr>
    <td width="208px">RscNativeTypecheck</td>
    <td width="208px">299.959 ms</td>
    <td width="208px">239.156 ms</td>
  </tr>
  <tr>
    <td>RscTypecheck</td>
    <td>467.265 ± 0.860 ms</td>
    <td>33.650 ± 0.016 ms</td>
  </tr>
  <tr>
    <td>ScalacTyper211</td>
    <td>4295.242 ± 24.084 ms</td>
    <td>707.156 ± 1.441 ms</td>
  </tr>
  <tr>
    <td>ScalacTyper212</td>
    <td>5167.287 ± 24.531 ms</td>
    <td>610.896 ± 1.594 ms</td>
  </tr>
</table>

<table>
  <th>
    <td>Cold</td>
    <td>Hot</td>
  </th>
  <tr>
    <td width="208px">ScalacCompile211</td>
    <td width="208px">8047.402 ± 43.037 ms</td>
    <td width="208px">1702.511 ± 10.349 ms</td>
  </tr>
  <tr>
    <td>ScalacCompile212</td>
    <td>9456.717 ± 45.414 ms</td>
    <td>1630.761 ± 10.607 ms</td>
  </tr>
  <tr>
    <td>JavacCompile</td>
    <td>801.029 ± 4.258 ms</td>
    <td>73.772 ± 0.153 ms</td>
  </tr>
</table>

## Comments

  * First and foremost, the current prototype of Rsc is significantly faster
    than full Scalac in both cold and hot benchmarks. Most impressively,
    hot typechecking is ~20x faster
    [with the disclaimer provided above](performance.md#disclaimer).
  * Scala Native has clearly succeeded in its goal of speeding
    up startup time of Scala applications. In cold benchmarks that are
    representative of running programs in command line, Rsc Native has a
    up to a ~4x edge over vanilla Rsc.
  * In the benchmarks above, all compilers are run in single-threaded mode.
    However, unlike Scalac and Javac that are inherently single-threaded,
    [Rsc was designed to enable massive parallelism](compiler.md). In the near
    future, we plan to leverage this unique feature of Rsc and parallelize
    its pipeline.
  * Finally, it was interesting to see that the current prototype of Rsc
    typechecks re2s significantly faster than Javac compiles re2j,
    [given the disclaimer provided above](performance.md#disclaimer).
    As we will be adding more features to Rsc, we will be keeping an eye on how
    this will affect compilation performance relative to Javac.
