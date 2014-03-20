hpc-coveralls [![Build Status](https://travis-ci.org/guillaume-nargeot/hpc-coveralls.png?branch=master)](https://travis-ci.org/guillaume-nargeot/hpc-coveralls)
=============

Coveralls support for Haskell code coverage.

hpc-coveralls currently only supports Travis CI, but support for other services will be added over time.

# Usage

## Travis CI

Commands to add to your project `.travis.yml`:
```yaml
before_install:
  - git clone https://github.com/guillaume-nargeot/hpc-coveralls.git
  - cd hpc-coveralls
  - cabal install
  - chmod +x run-cabal-test.sh hpc-coveralls.sh
script:
  - cabal configure --enable-tests --enable-library-coverage && cabal build
  - hpc-coveralls/run-cabal-test.sh
after_script:
  - hpc-coveralls/hpc-coveralls.sh [your-test-suite-name]
```

As you may notice, `cabal test` is replaced by the script `run-cabal-test.sh`.
The reason for this is explained in the next section.

For an example usage, please refer to [this-project](https://github.com/guillaume-nargeot/project-euler-haskell) .

# The run-cabal-test.sh script

When using HPC 0.6, `cabal test` outputs an error message and exits with the error code `1`, which prevents the build from succeeding.

In order for the build to succeed, hpc-coveralls provides the `run-cabal-test.sh` script which runs `cabal test` and return with `0` if the regex `^Test suite .\*: FAIL$` never matches any line of the output.
You can adapt this script for your needs, which may differ based on the testing framework you use.

The HPC issue should be fixed in version 0.7, which is provided by GHC 7.8 (Travis CI currently only provides GHC 7.6).

# Limitations

As Coveralls doesn't support yet partial-line coverage, the following convention is used to represent line coverage with line hit counts:
- `0` : the line is never hit,
- `1` : the line is partially covered,
- `2` : the line is fully covered.

(this convention is the same as the one used by [cloverage](https://github.com/lshift/cloverage) coveralls output)

There's an [open issue](https://github.com/lemurheavy/coveralls-public/issues/216) to improve this.
