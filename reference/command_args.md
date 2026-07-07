# Command line arguments

Make a new
[scribeCommandArgs](https://jmbarbone.github.io/scribe/reference/scribeCommandArgs-class.md)
object

## Usage

``` r
command_args(
  x = NULL,
  include = getOption("scribe.include", c("help", "version", NA_character_)),
  string = NULL,
  super = include
)
```

## Arguments

- x, string:

  Command line arguments; see
  [`base::commandArgs()`](https://rdrr.io/r/base/commandArgs.html) for
  default. At least one parameter has to be `NULL`. When `string` is
  `NULL`, `x` is used, which defaults to
  `commandArgs(trailingOnly = TRUE)`. Otherwise the value of `x` is
  converted to a `character`. If `string` is not `NULL`,
  [`scan()`](https://rdrr.io/r/base/scan.html) will be used to split the
  value into a `character` vector.

- include:

  Special default arguments to included. See `$initialize()` in
  [scribeCommandArgs](https://jmbarbone.github.io/scribe/reference/scribeCommandArgs-class.md)
  for more details.

- super:

  When `TRUE` the
  [scribeCommandArgs](https://jmbarbone.github.io/scribe/reference/scribeCommandArgs-class.md)
  object will be initialized with standard *super* arguments (e.g.,
  `---help`, `---version`)

## Value

A
[scribeCommandArgs](https://jmbarbone.github.io/scribe/reference/scribeCommandArgs-class.md)
object

## See also

Other scribe:
[`new_arg()`](https://jmbarbone.github.io/scribe/reference/new_arg.md),
[`scribeArg-class`](https://jmbarbone.github.io/scribe/reference/scribeArg-class.md),
[`scribeCommandArgs-class`](https://jmbarbone.github.io/scribe/reference/scribeCommandArgs-class.md)

## Examples

``` r
command_args()
#> Initial call:  
#> w Call $resolve() or $parse() to resolve arguments
#> Argument [--help] : FALSE
#> Argument [--version] : FALSE
command_args(c("-a", 1, "-b", 2))
#> Initial call:  -a 1 -b 2
#> w Call $resolve() or $parse() to resolve arguments
#> Argument [--help] : FALSE
#> Argument [--version] : FALSE
command_args(string = "-a 1 -b 2")
#> Initial call:  -a 1 -b 2
#> w Call $resolve() or $parse() to resolve arguments
#> Argument [--help] : FALSE
#> Argument [--version] : FALSE
```
