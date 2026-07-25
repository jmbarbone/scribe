# Command line arguments

Make a new
[scribeCommandArgs](https://jmbarbone.github.io/scribe/reference/scribeCommandArgs-class.md)
object

## Usage

``` r
command_args(
  x = commandArgs(trailingOnly = TRUE),
  include = getOption("scribe.include", c("help", "version", NA_character_)),
  super = include,
  string
)
```

## Arguments

- x:

  Command line arguments as a character vector. Use
  [`I()`](https://rdrr.io/r/base/AsIs.html) to pass a single (combined)
  string of arguments.

- include:

  Special default arguments to included. See `$initialize()` in
  [scribeCommandArgs](https://jmbarbone.github.io/scribe/reference/scribeCommandArgs-class.md)
  for more details.

- super:

  When `TRUE` the
  [scribeCommandArgs](https://jmbarbone.github.io/scribe/reference/scribeCommandArgs-class.md)
  object will be initialized with standard *super* arguments (e.g.,
  `---help`, `---version`)

- string:

  Deprecated. Use `command_args(I())` to pass a single string of
  arguments.

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
command_args(I("-a 1 -b 2"))
#> Initial call:  -a 1 -b 2
#> w Call $resolve() or $parse() to resolve arguments
#> Argument [--help] : FALSE
#> Argument [--version] : FALSE
```
