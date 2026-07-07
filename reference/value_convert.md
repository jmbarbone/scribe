# Simple conversions

Convert character to data types

## Usage

``` r
value_convert(x, to = default_convert)

scribe_convert(method = c("default", "evaluate", "none"))
```

## Arguments

- x:

  A vector of character values

- to:

  What to convert `x` to (see details for more)

- method:

  The conversion method:

  - `TRUE` or `"default"`: uses `value_convert()`

  - `"evaluate"` executes the string as an expression

  - `FALSE` or `NA` does nothing

  - When passed a `function`, simply returns the function

## Value

- `value_convert()`: A parsed value from `x`

&nbsp;

- `scribe_convert()`: A function that takes a argument `x` and converts
  it

## Details

`to` can be one of several values. Firstly the default of `default`
calls several additional functions that attempt to resolve a
transformation from a `character` vector to a different type. It is
recommended for users to enter their own specifications instead.
Secondly, a `function` (with a single argument) can be passed which will
then be applied directly to `x`. Third, a *prototype* value can be
passed. This might be risky for special types. Here, the values of
[`mode()`](https://rdrr.io/r/base/mode.html),
[`storage.mode()`](https://rdrr.io/r/base/mode.html),
[`attributes()`](https://rdrr.io/r/base/attributes.html), and
[`class()`](https://rdrr.io/r/base/class.html) are captured and
reassigned from `to` to `x`. A special check is implemented for
`factor`s to more safely convert. Lastly, `NULL` will do nothing and
will simply return `x`.

## Examples

``` r
str(value_convert("2023-03-05", as.Date))
#>  Date[1:1], format: "2023-03-05"
value_convert("a", factor(letters))
#> [1] a
#> Levels: a b c d e f g h i j k l m n o p q r s t u v w x y z
```
