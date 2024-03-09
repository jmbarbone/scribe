# scribe (development version)

- `---help`, `---version` _super_ arguments (`scribeSuperArg` objects) are now included with `scribeCommandArgs`.
This are objects that (by default) can be called within any `scribeCommandArgs` and intended to hold additional information about the `{scribe}` package.
These are not meant to be user accessible.
  - `scribeCommandArgs` gain a new `field`, `supers`; a list of `scribeSuperArg` objects
  - `command_args(super = included)` added
  - internal `scribe_version_arg()` function (used with `command_args(include = "verison")` and called with `--version`) is now deprecated.  Using `---version` now returns the version of `{scribe}`
  - internal `scribe_help_super()` now available via `---help` and returns information about `{scribe}`
  `ca$get_args(super = TRUE)` returns `scribeSuerArg`s
- `ca$help()` printing has been improved
- corrects issue with `ca$parse()`
- `ca$get_values(empty, super, included)` added to prevent filtering of specific argument types and values
- internal linting improvements
- `new_arg()` now throws a more helpful error when _value doesn't convert to itself_
- `arg$show()` now denotes if the argument is resolves by display an `"R"` before the value
- `arg$show()` now prints values of class `"scribe_empty_value"` as `<empty>`

# scribe 0.3.0

## Breaking changes

- `$convert` field now defaults to the newly exported `scribe_convert()` helper
  - This selects one of three conversions: 1) default (see next bullet), 2) string evaluation, and 3) no conversion.
  - default conversions use `value_convert()`, which internally uses `utils::type.convert()` (and some additional steps for dates.  Be aware that `type.convert("1", as.is = TRUE)` will return integers, and a decimal should be included if a numeric is desired (e.g., `type.convert("1.", as.is = TRUE)`
  - previously, a prototype could be set (e.g., `convert = character()`), which will now fail but can be replaced with a simple function (e.g., `convert = as.character`).

## Bug fixes

- `convert` is no longer ignored when set in `scribeArg` [#70](https://github.com/jmbarbone/scribe/issues/70)

## New features

- `flag` action now accepts `NA` as a default [#67](https://github.com/jmbarbone/scribe/issues/67)

## Non-user facing changes

- GitHub workflow added to maintain version bumps on merge [`jmbarbone/actions/r-check-version`](https://github.com/jmbarbone/actions/blob/main/examples/r-check-version.yaml)

# scribe 0.2.0

## Fixes

- `--help` no longer fails when `scribeArg` has `length(info) > 1` [#59](https://github.com/jmbarbone/scribe/issues/59)

## New features

- `execute` is a new field for `scribeArg` where a function can be called [#63](https://github.com/jmbarbone/scribe/issues/63)
- `stop` is a new field for `scribeArg` which controls how further arguments are parsed and allows for early stops [#60](https://github.com/jmbarbone/scribe/issues/60)
- `options()` for `{scribe}` are now listed in `?scribe` documentation and set in `.onAttach()` [#57](https://github.com/jmbarbone/scribe/issues/57)
- `scribeArgs` can now be given a separate `scribeArg` as a default [#54](https://github.com/jmbarbone/scribe/issues/54)
- positional arguments now can have default values [#52](https://github.com/jmbarbone/scribe/issues/52)
- `scribeArgs` with `action = 'flag'` now accept `default = TRUE` [#55](https://github.com/jmbarbone/scribe/issues/55) and (when option `no = TRUE`) can also accept `NA` [#67](https://github.com/jmbarbone/scribe/issues/67) 

## Breaking

- `scribeArgs` with `action = "flag"` will now throw an `error` instead of a `warning` when `default` is not `logical(1)` [#68](https://github.com/jmbarbone/scribe/issues/68) 

# scribe 0.1.0

- Added a `NEWS.md` file to track changes to the package.
