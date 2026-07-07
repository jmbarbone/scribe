# scribe argument

ReferenceClass object for managing arguments

## Details

The scribeArg class sets specifications and controls for how command
line arguments are to be parsed. These are meant to be used in
conjunction with
[scribeCommandArgs](https://jmbarbone.github.io/scribe/reference/scribeCommandArgs-class.md)
and specifically with the
[Rscript](https://rdrr.io/r/utils/Rscript.html) utility. However, a use
can define their own scribeArg separately.

## Fields

- `aliases`:

  `[character]`  
  A vector to denote the argument's name

- `action`:

  `[character]`  
  An action for resolving the argument (see `default` for note on using
  another scribeArg object)

- `default`:

  `[ANY]`  
  A default value. This can be another scribeArg object. When that is
  the case, the default value and action are pass through from the other
  scribeArg object.

- `convert`:

  `[ANY]`  
  Passed to the `to` argument in
  [`value_convert()`](https://jmbarbone.github.io/scribe/reference/value_convert.md)

- `n`:

  `[integer]`  
  The length of the values

- `info`:

  `[character]`  
  Additional information about the argument when printed

- `options`:

  `[list]`  
  A named list of options (see **Options**)

- `positional`:

  `[logical]`  
  Indicator if the argument is *positional* (i.e., not preceded by a `-`
  or `--` command line argument)

- `resolved`:

  `[logical]`  
  Has the object been resolved

- `value`:

  `[ANY]`  
  The resolve value

- `stop`:

  `[character]`  
  `"none"`, `"hard"`, or `"soft"`

- `execute`:

  `[function]`  
  (For advanced use). A `function` to be evaluated along with the arg.
  The function can have no parameters, a single parameter for the
  scribeArg object, or accept the scribeArg object as its first
  argument, and the
  [scribeCommandArgs](https://jmbarbone.github.io/scribe/reference/scribeCommandArgs-class.md)
  object as its second. Both objects will be passed by position

## Methods

- `get_action()`:

  Retrieve action

- `get_aliases()`:

  Retrieve aliases

- `get_default()`:

  Retrieve the default value

- `get_help()`:

  Retrieve help information as a `character` vector

- `get_name(clean = TRUE)`:

  Retrieve names

  `clean`

  :   When `TRUE` removes `-`s from text

- `get_value()`:

  Retrieve the resolved value

- [`help()`](https://rdrr.io/r/utils/help.html):

  Print out formatted help information

- `initialize( aliases = "", action = arg_actions(), default = NULL, convert = scribe_convert(), n = NA_integer_, info = NA_character_, options = list(), stop = c("none", "hard", "soft"), execute = invisible )`:

  Initialize the scribeArg object

  See **fields** for parameter information.

- `is_resolved()`:

  Check if object has been resolved

## Options

Several available options

- `action="list"`:

  `choices`

  :   An explicit set of values that argument must be. If the value
      parsed is not one of these, an error will occur.

- `action="flag"`:

  `no`

  :   When `TRUE` included appends `--no` to aliases to invert results  
      **Example:**  
      With the argument `new_arg("--test", options = list(no = TRUE))`,
      passing command arguments `--test` would set this to `TRUE` and
      `--no-test` explicitly set to `FALSE`.

## See also

Other scribe:
[`command_args()`](https://jmbarbone.github.io/scribe/reference/command_args.md),
[`new_arg()`](https://jmbarbone.github.io/scribe/reference/new_arg.md),
[`scribeCommandArgs-class`](https://jmbarbone.github.io/scribe/reference/scribeCommandArgs-class.md)

## Examples

``` r
# new_arg() is recommended over direct use of scribeArg$new()

# arguments with `--` indicators
new_arg("--verbose", action = "flag")
#> Argument [--verbose --no-verbose] : FALSE
new_arg(c("-f", "--force"), action = "flag")
#> Argument [-f --force --no-force] : FALSE
new_arg("--values", action = "list")
#> Argument [--values] : <null>

# positional
new_arg("verbose", action = "flag")
#> Argument [verbose no-verbose] : FALSE
new_arg("value", action = "list", n = 1)
#> Argument [value] : <null>

# special `...` action which absorbs left-over arguments
new_arg("values", action = "dots", info = "list of values")
#> Argument [values] : <null>
new_arg("...", info = "list of values") # defaults when alias is "..."
#> Argument [...] : <null>
```
