
<!-- README.md is generated from README.Rmd. Please edit that file -->

# scribe

<!-- badges: start -->

[![R-CMD-check](https://github.com/jmbarbone/scribe/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/jmbarbone/scribe/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/jmbarbone/scribe/branch/main/graph/badge.svg)](https://app.codecov.io/gh/jmbarbone/scribe?branch=main)
<!-- badges: end -->

The goal of scribe is to provide a detailed argument parser for
`Rscript`. This package contains no dependencies outside of `base` and
`methods`. The core functions utilize `ReferenceClasses` under the hood.

``` r
library(scribe)
```

Install `{scribe}` from CRAN with:

``` r
install.packages("scribe")
```

Alternatively, you can install the development version of `{scribe}`
[GitHub](https://github.com/) with:

    # install.packages("devtools")
    devtools::install_github("jmbarbone/scribe")

You can enter command arguments as a vector to test out the behavior.
Arguments can be added to the `scribeCommandArgs` class (here as `ca`).
Default behavior tries to parse objects but additional control can be
taken.

``` r
ca <- command_args(c("-a", "1", "-b", "2"))
ca$add_argument("-a")
ca$add_argument("-b")
args <- ca$parse()

str(args$a + args$b)
#>  int 3
```

Control

``` r
# don't convert numbers
ca <- command_args(c("-a", "1", "-b", "1.0"))
ca$add_argument("-a", convert = character())
ca$add_argument("-b", convert = character())
ca$parse()
#> $a
#> [1] "1"
#> 
#> $b
#> [1] "1.0"

# convert numbers to integers
ca <- command_args(c("verbose", "1", "1.5", "1.9"))
ca$add_argument("verbose", action = "flag")
ca$add_argument("...", convert = integer())
ca$parse()
#> $verbose
#> [1] TRUE
#> 
#> $...
#> NULL

# use functions for more control
ca <- command_args(c("verbose", "12-9-2022", "12-10-2022"))
ca$add_argument("verbose", action = "flag")
ca$add_argument("...", convert = function(i) as.Date(i, "%m-%d-%Y"))
ca$parse()
#> $verbose
#> [1] TRUE
#> 
#> $...
#> NULL
```

You’ll probably use `{scribe}` within small scripts that can be called
from your favorite terminal. The example below uses a function to call
this file, but if it is added to your `PATH` you’d be able to call it
directly:

``` bash
r-file -a 1 -b 9
```

``` r
lines <- "
#! /usr/bin/env -S Rscript --vanilla 

library(scribe)
ca <- scribe::command_args()
ca$add_argument('-a', default = 1)
ca$add_argument('-b', default = 2)
args <- ca$parse()

foo <- function(a, b) {
  a + b
}

do.call(foo, args)
"

file <- tempfile()
writeLines(lines, file)

rscript <- function(x, args = character()) {
  args <- c("--vanilla", x, args)
  res <- system2("Rscript", args, stdout = TRUE)
  writeLines(res)
}

rscript(file)
#> [1] 3
rscript(file, "-a 0")
#> [1] 2
rscript(file, "-a 0 -b 10")
#> [1] 10
```

## Examples

I’ve been using `{scribe}` for some personal scripts. Below is a short
list of some examples (mostly in my
[`jmb`](https://github.com/jmbarbone/jmb) repo):

- [pak](https://github.com/jmbarbone/jmb/blob/main/bin/pak): call
  [`{pak}`](https://pak.r-lib.org/) from your terminal
- [update-r-pkgs](https://github.com/jmbarbone/jmb/blob/main/bin/update-r-pkgs):
  update old R packages
- [todos](https://github.com/jmbarbone/jmb/blob/main/bin/todos): calls
  [`mark::todos()`](https://jmbarbone.github.io/mark/reference/todos.html)
- [fixmes](https://github.com/jmbarbone/jmb/blob/main/bin/fixmes): calls
  [`mark::fixmes()`](https://jmbarbone.github.io/mark/reference/todos.html)

## Other packages

This isn’t the first package. Most contain other dependencies, some even
in different languages (e.g., `python`).

- [`{argparse}`](https://github.com/trevorld/r-argparse)
- [`{optparse}`](https://github.com/trevorld/r-optparse)
- [`{getopt}`](https://github.com/trevorld/r-getopt)
- [`{minimist}`](https://github.com/jeroen/minimist) (CRAN archived)
- [`{optigrab}`](https://github.com/decisionpatterns/optigrab)
- [`{docopt}`](https://github.com/docopt/docopt.R)
