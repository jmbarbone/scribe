# New command argument

Make a new
[scribeArg](https://jmbarbone.github.io/scribe/reference/scribeArg-class.md)
object

## Usage

``` r
new_arg(
  aliases = "",
  action = arg_actions(),
  default = NULL,
  convert = scribe_convert(),
  n = NA_integer_,
  info = NULL,
  options = list(),
  stop = c("none", "hard", "soft"),
  execute = invisible
)
```

## Arguments

- aliases, action, convert, options, default, info, n, stop, execute:

  See `$initialize()` in
  [scribeArg](https://jmbarbone.github.io/scribe/reference/scribeArg-class.md).

## Value

A
[scribeArg](https://jmbarbone.github.io/scribe/reference/scribeArg-class.md)
object

## See also

Other scribe:
[`command_args()`](https://jmbarbone.github.io/scribe/reference/command_args.md),
[`scribeArg-class`](https://jmbarbone.github.io/scribe/reference/scribeArg-class.md),
[`scribeCommandArgs-class`](https://jmbarbone.github.io/scribe/reference/scribeCommandArgs-class.md)

## Examples

``` r
new_arg()
#> Argument [] : <null>
new_arg("values", action = "dots")
#> Argument [values] : <null>
new_arg(c("-f", "--force"), action = "flag")
#> Argument [-f --force --no-force] : FALSE
```
