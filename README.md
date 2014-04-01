hpc-coveralls [![Build Status](https://travis-ci.org/guillaume-nargeot/hpc-coveralls.png?branch=master)](https://travis-ci.org/guillaume-nargeot/hpc-coveralls)
=============

hpc-coveralls converts and sends Haskell projects hpc code coverage to [coverall.io](http://coveralls.io/).

At the moment, only [Travis CI](http://travis-ci.org) is supported, but other CI services will be supported soon.

hpc-coveralls is still under development and any contributions are welcome!

# Usage

## Travis CI

Commands to add to your project `.travis.yml`:
```yaml
before_install:
  - cabal install hpc-coveralls
script:
  - cabal configure --enable-tests --enable-library-coverage && cabal build
  - run-cabal-test [optional-cabal-test-arguments]
after_script:
  - hpc-coveralls [your-test-suite-name]
```

Note that the usual `cabal test` command is replaced by `run-cabal-test`.
The reason for this is explained in the next section.

For an example usage, please refer to [this-project](https://github.com/guillaume-nargeot/project-euler-haskell) `.travis.yml` file ([result on coveralls](https://coveralls.io/r/guillaume-nargeot/project-euler-haskell)).

## The run-cabal-test command

When using hpc 0.6, `cabal test` outputs an error message and exits with the error code `1`, which results in a build failure.

In order to prevent this from happening, hpc-coveralls provides the `run-cabal-test` command which runs `cabal test` and returns with `0` if the regex `^Test suite .*: FAIL$` never matches any line of the output.

This hpc issue should be fixed in version 0.7 (not yet available on Travis CI).

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


[![Bitdeli Badge](https://d2weczhvl823v0.cloudfront.net/guillaume-nargeot/hpc-coveralls/trend.png)](https://bitdeli.com/free "Bitdeli Badge")

