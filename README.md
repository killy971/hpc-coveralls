hpc-coveralls [![Build Status](https://travis-ci.org/guillaume-nargeot/hpc-coveralls.png?branch=master)](https://travis-ci.org/guillaume-nargeot/hpc-coveralls)
=============

hpc-coveralls converts and sends Haskell projects hpc code coverage to [coverall.io](http://coveralls.io/).

At the moment, only [Travis CI](http://travis-ci.org) has been tested, but hpc-coveralls should be compatible with other CI services (Check `HpcCoverallsMain` [source](https://github.com/guillaume-nargeot/hpc-coveralls/blob/master/src/HpcCoverallsMain.hs) for the list).

hpc-coveralls is still under development and any contributions are welcome!

# Usage

## Travis CI

Commands to add to your project `.travis.yml` when using GHC 7.8:
```yaml
before_install:
  - cabal install hpc-coveralls
script:
  - cabal configure --enable-tests --enable-library-coverage && cabal build && cabal test
after_script:
  - hpc-coveralls [options] [test-suite-name]
```

When using a GHC version prior to 7.8, you have to replace the `cabal test` command by `run-cabal-test`, as in the following example:
```yaml
before_install:
  - cabal install hpc-coveralls
script:
  - cabal configure --enable-tests --enable-library-coverage && cabal build
  - run-cabal-test [options] [cabal-test-options]
after_script:
  - hpc-coveralls [options] [test-suite-name]
```

The reason for this is explained in the next section.

For a real world example usage, please refer to [this-project](https://github.com/guillaume-nargeot/project-euler-haskell) `.travis.yml` file ([result on coveralls](https://coveralls.io/r/guillaume-nargeot/project-euler-haskell)).
You can also refer to the `.travis.yml` file of hpc-coveralls itself, which is configured with [multi-ghc-travis](https://github.com/hvr/multi-ghc-travis).

## The run-cabal-test command

When using hpc 0.6, `cabal test` outputs an error message and exits with the error code `1`, which results in a build failure.

In order to prevent this from happening, hpc-coveralls provides the `run-cabal-test` command which runs `cabal test` and returns with `0` if the regex `^Test suite .*: FAIL$` never matches any line of the output.

As this issue is fixed in the hpc version shipped with GHC 7.8, you don't have to use `run-cabal-test` when testing with GHC 7.8 and can safely use `cabal test`.

### Options

The `--cabal-name` option can be used to specify a custom executable name instead of the default `cabal` when calling `cabal test`.<br/>
Below is an example which can be useful for projects with a Travis configuration based on [multi-ghc-travis](https://github.com/hvr/multi-ghc-travis):

```yaml
run-cabal-test --cabal-name=cabal-1.18
```

## The hpc-coveralls command

At the moment, you can specify only one suite. For example, if your test suite is named `test-all`, use the command as follows:

```yaml
hpc-coveralls test-all
```

### Options

The `--exclude-dir` option can be used to exclude source files located under a given directory from the coverage report.<br/>
You can exclude source files located under the `test/` by using this option as in the following example:

```yaml
hpc-coveralls --exclude-dir=test [test-suite-name]
```

You can specify multiple excluded folders by using the following example syntax:

```yaml
hpc-coveralls --exclude-dir=test1 --exclude-dir=test2 [test-suite-name]
```

# Limitations

As Coveralls doesn't support yet partial-line coverage, the following convention is used to represent line coverage with line hit counts:
- `0` : the line is never hit,
- `1` : the line is partially covered,
- `2` : the line is fully covered.

This convention is the same as the one used by [cloverage](https://github.com/lshift/cloverage) coveralls output for Clojure projects code coverage.

There's an [open issue](https://github.com/lemurheavy/coveralls-public/issues/216) to improve this.

# Contributing

hpc-coveralls is still under development and any contributions are welcome!

[Future Plans and Ideas](https://github.com/guillaume-nargeot/hpc-coveralls/wiki/Future-Plans-and-Ideas)

# License

BSD3 ([tl;dr](https://tldrlegal.com/license/bsd-3-clause-license-(revised)))

# Notes

- HPC publication: http://ittc.ku.edu/~andygill/papers/Hpc07.pdf
