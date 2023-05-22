# scribe 0.2.0

## Fixes

-   `--help` no longer fails when `scribeArg` has `length(info) > 1` [#59](https://github.com/jmbarbone/scribe/issues/59)

## New features

-   `execute` is a new field for `scribeArg` where a function can be called [#63](https://github.com/jmbarbone/scribe/issues/63)
-   `stop` is a new field for `scribeArg` which controls how further arguments are parsed and allows for early stops [#60](https://github.com/jmbarbone/scribe/issues/60)
-   `options()` for `{scribe}` are now listed in `?scribe` documentation and set in `.onAttach()` [#57](https://github.com/jmbarbone/scribe/issues/57)
-   `scribeArgs` can now be given a separate `scribeArg` as a default [#54](https://github.com/jmbarbone/scribe/issues/54)
-   positional arguments now can have default values [#52](https://github.com/jmbarbone/scribe/issues/52)
-   `scribeArgs` with `action = 'flag'` now accept `default = TRUE` [#55](https://github.com/jmbarbone/scribe/issues/55)

# scribe 0.1.0

-   Added a `NEWS.md` file to track changes to the package.
