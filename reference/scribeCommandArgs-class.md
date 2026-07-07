# scribe command arguments

Reference class object for managing command line arguments.

## Details

This class manages the command line argument inputs when passed via the
[Rscript](https://rdrr.io/r/utils/Rscript.html) utility. Take the simple
script below which adds two numbers, which we will save in an executable
file called `add.R`,

    #!/usr/bin/env Rscript

    library(scribe)
    ca <- command_args()
    ca$add_argument("--value1", default = 0L)
    ca$add_argument("--value2", default = 0L)
    args <- ca$parse()
    writeLines(args$value1 + args$value2)

When called by a terminal, we can pass arguments and return a function.

    add.R --value1 10 --value2 1
    11

When testing, you can simulate command line arguments by passing them
into the `input` field. By default, this will grab values from
[`base::commandArgs()`](https://rdrr.io/r/base/commandArgs.html), so use
with the [Rscript](https://rdrr.io/r/utils/Rscript.html) utility doesn't
require any extra steps.

Most methods are designed to return `.self`, or the scribeCommandArgs
class. The exceptions to these are the the `$get_*()` methods, which
return their corresponding values, and `$parse()` which returns a named
`list` of the parsed input values.

## Fields

- `input`:

  `[character]`  
  A character vector of command line arguments. See also
  [`command_args()`](https://jmbarbone.github.io/scribe/reference/command_args.md)

- `values`:

  `[list]`  
  A named `list` of values. Empty on initialization and populated during
  argument resolving.

- `args`:

  `[list]`  
  A List of
  [scribeArg](https://jmbarbone.github.io/scribe/reference/scribeArg-class.md)s

- `description`:

  `[character]`  
  Additional help information

- `included`:

  `[character]`  
  Default
  [scribeArg](https://jmbarbone.github.io/scribe/reference/scribeArg-class.md)s
  to include

- `examples`:

  `[character]`  
  Examples to print with help

- `comments`:

  `[character]`  
  Comments printed with

- `resolved`:

  `[logical]`  
  A `logical` value indicated if the `$resolve()` method has been
  successfully executed.

- `working`:

  `[character]`  
  A copy of `input`. Note: this is used to track parsing progress and is
  not meant to be accessed directly.

- `stop`:

  `[character]`  
  Determines parsing

- `supers`:

  `[list]`  
  A list of `scribeSuperArgs`s

## Methods

- `add_argument( ..., action = arg_actions(), default = NULL, convert = scribe_convert(), n = NA_integer_, info = NULL, options = list(), stop = c("none", "hard", "soft"), execute = invisible )`:

  Add a
  [scribeArg](https://jmbarbone.github.io/scribe/reference/scribeArg-class.md)
  to `args`

  `...`

  :   Either aliases or a
      [scribeArg](https://jmbarbone.github.io/scribe/reference/scribeArg-class.md).
      If the latter, all other arguments are ignored. Note that only the
      first value ([..1](https://rdrr.io/r/base/dots.html)) is used.

  `action`, `options`, `convet`, `default`, `n`, `info`, `stop`, `execute`

  :   See
      [`new_arg()`](https://jmbarbone.github.io/scribe/reference/new_arg.md)

- `add_description(..., sep = "")`:

  Add a value to `description`

  `...`

  :   Information to paste into the description

  `sep`

  :   `character` separate for `...`

- `add_example(x, comment = "", prefix = "$ ")`:

  Add a value to `examples`

  `x`

  :   A code example as a `character`

  `comment`

  :   An optional comment to append

  `prefix`

  :   An optional prefix for the example

- `get_args(included = TRUE, super = FALSE)`:

  Retrieve `args`

  `included`

  :   If `TRUE` also returns included default
      [scribeArg](https://jmbarbone.github.io/scribe/reference/scribeArg-class.md)s
      defined in `$initialize()`

  `super`

  :   If `TRUE` also returns super args

- `get_description()`:

  Retrieve `description`

- `get_examples()`:

  Retrieve `examples`

- `get_input()`:

  Retrieve `input`

- `get_values(empty = FALSE, super = FALSE, included = FALSE)`:

  Retrieve `values`

  `{empty}`

  :   If `TRUE` returns empty values, too

  `super`

  :   If `TRUE` also returns values from super args

  `included`

  :   If `TRUE` also returns included default
      [scribeArg](https://jmbarbone.github.io/scribe/reference/scribeArg-class.md)s
      defined in `$initialize()`

- [`help()`](https://rdrr.io/r/utils/help.html):

  Print the help information

- `initialize( input = "", include = c("help", "version", NA_character_), supers = include )`:

  Initialize the scribeCommandArgs object. The wrapper
  [`command_args()`](https://jmbarbone.github.io/scribe/reference/command_args.md)
  is recommended rather than calling this method directly.

  `input`

  :   A `character` vector of command line arguments to parse

  `include`

  :   A character vector denoting which default
      [scribeArg](https://jmbarbone.github.io/scribe/reference/scribeArg-class.md)s
      to include in `args`

  `supers`

  :   A character vector denoting which default `scribeSuperArg`s to
      include in `supers` (i.e., arguments called with \`—\` prefixes)

- [`parse()`](https://rdrr.io/r/base/parse.html):

  Return a named `list` of parsed values of from each
  [scribeArg](https://jmbarbone.github.io/scribe/reference/scribeArg-class.md)
  in `args`

- `resolve()`:

  Resolve the values of each
  [scribeArg](https://jmbarbone.github.io/scribe/reference/scribeArg-class.md)
  in `args`. This method is called prior to \$parse()

- `set_description(..., sep = "")`:

  Set the value of `description`

  `...`

  :   Information to paste into the description

  `sep`

  :   `character` separate for `...`

- `set_example(x = character(), comment = "", prefix = "$ ")`:

  Set the value of `examples`

  `x`

  :   A code example as a `character`

  `comment`

  :   An optional comment to append

  `prefix`

  :   An optional prefix for the example

- `set_input(value)`:

  Set `input`. Note: when called, `resolved` is (re)set to `FALSE` and
  values need to be parsed again.

- `set_values(i = TRUE, value)`:

  Set `values`

  `i`

  :   Index value of `working` to set

  `value`

  :   The value to set

- [`version()`](https://rdrr.io/r/base/Version.html):

  Print the
  [scribe-package](https://jmbarbone.github.io/scribe/reference/scribe-package.md)
  version

## See also

Other scribe:
[`command_args()`](https://jmbarbone.github.io/scribe/reference/command_args.md),
[`new_arg()`](https://jmbarbone.github.io/scribe/reference/new_arg.md),
[`scribeArg-class`](https://jmbarbone.github.io/scribe/reference/scribeArg-class.md)

## Examples

``` r
# command_args() is recommended over direct use of scribeCommandArgs$new()

ca <- command_args(c(1, 2, 3, "--verbose"))
ca$add_argument("--verbose", action = "flag")
ca$add_argument("...", "values", info = "values to add", default = 0.0)
args <- ca$parse()

if (args$verbose) {
  message("Adding ", length(args$values), " values")
}
#> Adding 3 values

sum(args$values)
#> [1] 6

# $parse() returns a named list, which means scribeCommandArgs can function
# as a wrapper for calling R functions inside Rscript

ca <- command_args(c("mean", "--size", 20, "--absolute"))
ca$add_argument("fun", action = "list")
ca$add_argument("--size", default = 5L)
ca$add_argument("--absolute", action = "flag")
args <- ca$parse()

my_function <- function(fun, size, absolute = FALSE) {
  fun <- match.fun(fun)
  x <- sample(size, size, replace = TRUE)
  res <- fun(x)
  if (absolute) res <- abs(res)
  res
}

do.call(my_function, args)
#> [1] 8.6
```
