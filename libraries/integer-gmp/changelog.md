# Changelog for [`integer-gmp` package](http://hackage.haskell.org/package/integer-gmp)

## 1.1 *January 2021**

  * integer-gmp is now a shallow backward compatibility package on top of
    ghc-bignum

## 1.0.3.0 *January 2019*

  * Bundled with GHC 8.10.1

  * Documentation changes

## 1.0.2.0 *April 2018*

  * Bundled with GHC 8.4.2

  * Define `powModSecInteger`, a "secure" version of `powModInteger` using the
    `mpz_powm_sec` function.

## 1.0.1.0 *July 2017*

  * Bundled with GHC 8.2.1

  * Optimize `minusInteger`

  * Fix the right-shift operation for negative big integers (fixes GHC #12136)

  * Make operations more strict

## 1.0.0.1  *Feb 2016*

  * Bundled with GHC 8.0.1

  * Optimise `bitBigNat`/`bigInteger`

  * Fix minor issue in `eqBigNatWord#`

  * Various minor optimisations

## 1.0.0.0  *Mar 2015*

  * Bundled with GHC 7.10.1

  * Complete rewrite of `integer-gmp`. For more details, see
    https://gitlab.haskell.org/ghc/ghc/wikis/design/integer-gmp2

## 0.5.1.0  *Feb 2014*

  * Bundled with GHC 7.8.1

  * Improved Haddock documentation

  * New [PrimBool](https://gitlab.haskell.org/ghc/ghc/wikis/prim-bool)
    versions of comparison predicates in `GHC.Integer`:

        eqInteger# :: Integer -> Integer -> Int#
        geInteger# :: Integer -> Integer -> Int#
        gtInteger# :: Integer -> Integer -> Int#
        leInteger# :: Integer -> Integer -> Int#
        ltInteger# :: Integer -> Integer -> Int#
        neqInteger# :: Integer -> Integer -> Int#

  * New `GHC.Integer.testBitInteger` primitive for use with `Data.Bits`

  * Reduce short-lived heap allocation and try to demote `J#` back
    to `S#` more aggressively.  See also
    [#8647](https://gitlab.haskell.org/ghc/ghc/issues/8647)
    for more details.

  * New GMP-specific binary (de)serialization primitives added to
    `GHC.Integer.GMP.Internals`:

        importIntegerFromByteArray
        importIntegerFromAddr
        exportIntegerToAddr
        exportIntegerToMutableByteArray
        sizeInBaseInteger

  * New GMP-implemented number-theoretic operations added to
    `GHC.Integer.GMP.Internals`:

        gcdExtInteger
        nextPrimeInteger
        testPrimeInteger
        powInteger
        powModInteger
        powModSecInteger
        recipModInteger
