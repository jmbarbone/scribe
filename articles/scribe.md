# scribe

``` r

library(scribe)
```

The [scribe](https://jmbarbone.github.io/scribe/) package provides a
means of defining command line argument inputs for use with the
[Rscript](https://search.r-project.org/R/refmans/utils/html/Rscript.html)
utility. Users will primarily use the
[`command_args()`](https://jmbarbone.github.io/scribe/reference/command_args.md)
function to capture the command line arguments and initialize a
`scribeCommandArgs` object. A `scribeCommandArgs` is a [**Reference
Class**](https://search.r-project.org/R/refmans/methods/html/refClass.html)
object with methods to configure how to parse these arguments for use
within **R**.

Let’s look at to use the [scribe](https://jmbarbone.github.io/scribe/)
class first. Our goal is to wrap a simple function to generate a
sequence of integers or letters. Here we build out a `scribeCommandArgs`
object, add a couple of arguments with the `$add_argument()` method,
then parse into a named list with `$parse()`.

``` r

ca <- command_args(c("-n", "5", "--method", "numbers"))
ca$add_argument("-n", default = 1L)
ca$add_argument("--method", default = "letters")
args <- ca$parse()

out <- seq_len(args$n)

method <- match.arg(args$method, c("letters", "numbers"))
if (method == "letters") {
  out <- letters[out]
}

out
#> [1] 1 2 3 4 5
```

In the example above we specify what the command line arguments are
within
[`command_args()`](https://jmbarbone.github.io/scribe/reference/command_args.md).
The intended utility of this is to capture these arguments when passe
within an `Rscript` file. Below is the same structure, but as we would
expect from within a script intended to be called from a command line.
[`command_args()`](https://jmbarbone.github.io/scribe/reference/command_args.md)
will grab whatever command line arguments are passed to the script.

``` r

#!/usr/bin/env Rscript
# filename: seq_len.R

library(scribe)
ca <- command_args()
ca$add_argument("-n", default = 1L)
ca$add_argument("--method", default = "letters")
args <- ca$parse()

out <- seq_len(args$n)

method <- match.arg(args$method, c("letters", "numbers"))
if (method == "letters") {
  out <- letters[out]
}

out
```

    seq_len.R -n 3 
    #> [1] "a" "b" "c"

    seq_len.R -n 3 --method numbers
    #> [1] 1 2 3

One way I like to use [scribe](https://jmbarbone.github.io/scribe/) is
by passing the values directly to another function via
[`do.call()`](https://rdrr.io/r/base/do.call.html).

Two examples provided that find a specified dataset and then perform
something to it. Were I to use this personally, I would probably pass a
file path and use a read function first, rather than the
[`get()`](https://rdrr.io/r/base/get.html) function.

``` r

my_summary <- function(data, levels = 7, sig_figs = 3, q_type = 7) {
  data <- get(data, mode = "list")
  stopifnot(is.data.frame(data))
  summary(data, maxsum = levels, digits = sig_figs, quantile.type = q_type)
}

my_model <- function(data, correlation = FALSE) {
  data <- get(data, mode = "list")
  stopifnot(is.data.frame(data))
  form <- stats::DF2formula(data)
  mod <- stats::lm(form, data)
  summary(mod, correlation = correlation)
}
```

``` r

ca <- command_args(string = "CO2 --levels 3 --sig-figs 2 --q-type 3")
ca$add_description("Summarise a dataset")
ca$add_argument(
  "data",
  info = "Name of the dataset to find"
)
ca$add_argument(
  "--levels",
  default = 7L,
  info = "Maximum number of levels shown for factors"
)
ca$add_argument(
  "--sig-figs",
  default = 3L,
  info = "Number of significant figures"
)
ca$add_argument(
  "--q-type",
  default = 7L,
  info = "Quantile type"
)
args <- ca$parse()
do.call(my_summary, args)
#>      Plant             Type         Treatment       conc          uptake    
#>  Qn1    : 7   Quebec     :42   nonchilled:42   Min.   :  95   Min.   : 7.7  
#>  Qn2    : 7   Mississippi:42   chilled   :42   1st Qu.: 175   1st Qu.:17.9  
#>  (Other):70                                    Median : 350   Median :28.1  
#>                                                Mean   : 435   Mean   :27.2  
#>                                                3rd Qu.: 675   3rd Qu.:37.1  
#>                                                Max.   :1000   Max.   :45.5
```

``` r

ca <- command_args(string = "attitude --correlation")
ca$add_argument(
  "data",
  info = "Name of the dataset to find"
)
ca$add_argument(
  "--correlation",
  action = "flag",
  info = "When set, prints the correlation matrix of estimated parameters"
)
args <- ca$parse()
do.call(my_model, args)
#> 
#> Call:
#> stats::lm(formula = form, data = data)
#> 
#> Residuals:
#>      Min       1Q   Median       3Q      Max 
#> -10.9418  -4.3555   0.3158   5.5425  11.5990 
#> 
#> Coefficients:
#>             Estimate Std. Error t value Pr(>|t|)    
#> (Intercept) 10.78708   11.58926   0.931 0.361634    
#> complaints   0.61319    0.16098   3.809 0.000903 ***
#> privileges  -0.07305    0.13572  -0.538 0.595594    
#> learning     0.32033    0.16852   1.901 0.069925 .  
#> raises       0.08173    0.22148   0.369 0.715480    
#> critical     0.03838    0.14700   0.261 0.796334    
#> advance     -0.21706    0.17821  -1.218 0.235577    
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Residual standard error: 7.068 on 23 degrees of freedom
#> Multiple R-squared:  0.7326, Adjusted R-squared:  0.6628 
#> F-statistic:  10.5 on 6 and 23 DF,  p-value: 1.24e-05
#> 
#> Correlation of Coefficients:
#>            (Intercept) complaints privileges learning raises critical
#> complaints -0.07                                                     
#> privileges -0.12       -0.37                                         
#> learning   -0.16       -0.30      -0.14                              
#> raises     -0.08       -0.52       0.08      -0.21                   
#> critical   -0.66        0.00      -0.02       0.20    -0.28          
#> advance     0.02        0.40      -0.18      -0.35    -0.43  -0.13
```
