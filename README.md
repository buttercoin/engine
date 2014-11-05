Buttercoin Engine
=================

#### Overview

This is a subset of the Buttercoin.com codebase for performing order matching, reporting market status, and generating notification resulting from trade execution. The project uses [SBT](http://www.scala-sbt.org/) and is composed of several sub-projects:

- `common` contains code that is or likely will be shared by other systems.
- `core` provides most of the engine functionality.
- `models` provides some data models and the notion of an engine snapshot. Parts of `core` should probably be moved here.
- `perf-testing` has a set of [ScalaMeter](http://scalameter.github.io/) micro-benchmarks.

This repo doesn't yet include mechanisims for handling user accounts, processing external transactions, connecting over HTTP, or many other things, since those are largely separable concerns.

#### Planned Work

- A demo container application that starts up the engine and accepts external connections
- `akka-persistence` support for write-ahead journaling
- Performance tuning

#### Tests

To run all unit tests (but not performance tests), simply run `sbt test`.

To run performance tests without the "GC detected." spew use:

    sbt "project perf-testing" "test-only -- -silent" | grep -v "GC detected."

#### Configuration

There are a couple of [Typesafe Config](https://github.com/typesafehub/config) files under resources which control currencies available in a locale and fee structure.

#### Versioning

This project will be maintained under the Semantic Versioning guidelines as much as possible.

Releases will be numbered with the follow format:

`<major>.<minor>.<patch>`

And constructed with the following guidelines:

- Breaking backward compatibility bumps the major (and resets the minor and patch)
- New additions without breaking backward compatibility bumps the minor (and resets the patch)
- Bug fixes and misc changes bumps the patch

For more information on SemVer, please visit http://semver.org/.

#### License

This repo is MIT licensed. We'll be adding a CLA soon.

&copy; 2014 Buttercoin. All Rights Reserved.
