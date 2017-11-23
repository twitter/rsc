<!-- Copyright (c) 2017 Twitter, Inc. -->
<!-- Licensed under the Apache License, Version 2.0 (see LICENSE.md). -->

# Performance

Our research goal is to achieve dramatic compilation speedups (5-10x)
for typical Scala codebases, and we are currently well on track to reaching
this goal.

## Disclaimer

At this point, Rsc only supports [a small subset of Scala](language.md).
Moreover, Rsc only loads stubs instead of fully loading metadata from
the standard library jars. Finally, [our typechecker](compiler.md) is just
a prototype that only resolves names instead of fully typechecking code

Therefore, performance numbers that you will see below may significantly
deteriorate as we will be implementing more and more functionality of
the Scala compiler. Nonetheless, we believe that these numbers are
very much worth sharing, as they represent a novel take on measuring
the limits of Scala compilation speed.

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
Scalac 2.11.11 and Javac 1.8.0_151.

Our benchmarks run different fragments of compilation pipelines of
different compilers on two comparable codebases:
  * [re2j](../examples/re2j/src/main/java/java/util/regex), an implementation
    of linear time regular expression matching in Java (11740 loc).
  * [re2s](../examples/re2s/src/main/scala/java/util/regex), a port of re2j
    to Scala [performed in Scala Native](https://github.com/scala-native/scala-native/pull/894).
    For Rsc, re2s is accompanied by [Stdlib.scala](../stdlib/src/main/scala/Stdlib.scala),
    a source file that declares stubs for referenced definitions from
    scala-library and the JDK (11028 loc + 182 loc = 11210 loc).

We have run five families of benchmarks:
  * RscNativeTypecheck: run the output of rscNative/nativeLink on re2s
    and Stdlib.scala with -Ystop-after:typecheck.
  * RscTypecheck: run the output of rscJVM/compile on re2s and Stdlib.scala
    with -Ystop-after:typecheck.
  * ScalacTypecheck: run Scalac on re2s
    with -d tempdir -usejavacp -Ystop-after:typer.
  * ScalacCompile: run Scalac on re2s
    with -d tempdir -usejavacp.
  * JavacCompile: run Javac on re2j
    with -d tempdir.

Every family of benchmarks was run in two different modes: cold (performance
of a single run without warm-up) and hot (performance of a steady state achieved
by doing hundreds or even thousands of runs).

To benchmark native applications, we used
[our own microbenchmark harness](../bench/jvm/src/main/scala/rsc/bench/CliBench.scala):
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

## Results

To reproduce, run `sbt bench` (this will take a while).

|                    | Cold                 | Hot                  |
|--------------------|----------------------|----------------------|
| RscNativeTypecheck | 300.146 ms           | 282.468 ms           |
| RscTypecheck       | 476.843 ± 0.822 ms   | 33.685 ± 0.024 ms    |
| ScalacNamer 2.12.4 | 1843.751 ± 14.930 ms | 29.825 ± 0.090 ms    |
| ScalacNamer 2.11.11| 1423.331 ± 37.056 ms | 71.100 ± 0.318 ms    |
| ScalacTypecheck    | 4326.812 ± 28.763 ms | 712.872 ± 3.554 ms   |
| ScalacCompile      | 8098.704 ± 47.390 ms | 1691.732 ± 12.205 ms |
| JavacCompile       | 851.787 ± 3.460 ms   | 76.164 ± 0.169 ms    |

## Comments
  * Compared to the Scala namer in Scala 2.11.11, our prototype type-checker is about 2x faster.
    However, compared to the namer in 2.12.4 we are about 10% slower.
  * First and foremost, we are happy to announce that typechecking in Rsc
    is currently way faster than in Scalac. Cold typechecking is ~9x faster and
    hot typechecking is a whopping ~21x faster at ~330kloc/s.
    **[The disclaimer above](performance.md#disclaimer) does apply, so these
    results should be viewed as work in progress rather than final numbers**.
  * Scala Native has clearly succeeded in its goal of speeding
    up startup time of Scala applications. In cold benchmarks that are
    representative of running programs in command line, Rsc Native has a
    ~1.6x edge over vanilla Rsc, which results in a ~14x total speedup
    over Scalac.
  * Somewhat surprisingly, both scanning and parsing in Rsc are faster than
    in Scalac 2.11.11 (~3.0mloc/s and ~1.3mloc/s vs ~2.5mloc/s and ~0.5mloc/s),
    even though Rsc follows the Scalameta principle of representing source code
    exactly as it's written.
  * In the benchmarks above, all compilers are run in single-threaded mode.
    However, unlike Scalac and Javac that are inherently single-threaded,
    [Rsc was built to enable massive parallelism](compiler.md). In the near
    future, we plan to leverage this unique feature of Rsc and parallelize
    its pipeline.
  * Finally, it was interesting to see that at this point Rsc typechecks re2s
    significantly faster than Javac compiles re2j. As we will be adding more
    features to Rsc, we will be keeping an eye on how this will affect
    compilation performance relative to Javac.
