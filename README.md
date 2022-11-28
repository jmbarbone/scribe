
<!-- README.md is generated from README.Rmd. Please edit that file -->

# scribe

<!-- badges: start -->
<!-- badges: end -->

The goal of scribe is to provide a detailed argument parser for
`Rscript`. This package contains no dependencies outside of `base` and
`methods`. The core functions utilize `ReferenceClasses` under the hood.

``` r
library(scribe)
```

Workflows with intention to support:

``` r
ca <- command_args(c("-a", "1", "-b", "2"))
ca$add_argument("-a", type = "integer")
#> Initial call:  -a 1 -b 2
#> Argument [-a] : <null>
ca$add_argument("-b", type = "integer")
#> Initial call:  -a 1 -b 2
#> Argument [-a] : <null>
#> Argument [-b] : <null>
args <- ca$parse()

str(args$a + args$b)
#>  int 3
```

``` r
lines <- "
#! /usr/bin/Rscript -S

requireNamespace('scribe')
ca <- scribe::command_args()
ca$add_argument('-a', default = 1)
ca$add_argument('-b', default = 2)
args <- ca$parse()

foo <- function(a, b) {
  a + b
}

print(foo(args$a, args$b))
print(do.call(foo, args))
"

file <- tempfile()
writeLines(lines, file)
system2("Rscript", c("--vanilla", file))
system2("Rscript", c("--vanilla", file, "-a 0"))
system2("Rscript", c("--vanilla", file, "-a 0 -b 10"))
```

## Other packages

This isn’t the first package. Most contain other dependencies, some even
in different langauges (e.g., `python`).

- [`{argparse}`](https://github.com/trevorld/r-argparse)
- [`{optparse}`](https://github.com/trevorld/r-optparse)
- [`{getopt}`](https://github.com/trevorld/r-getopt)
- [`{minimist}`](https://github.com/jeroen/minimist) (CRAN archived)
- [`{optigrab}`](https://github.com/decisionpatterns/optigrab)
- [`{docopt}`](https://github.com/docopt/docopt.R)
