#' Simple conversions
#'
#' Convert character to data types
#'
#' @param x A vector of character values
#' @param to What to convert `x` to (see details for more)
#'
#' @details `to` can be one of several values.  Firstly the default of `default`
#'   calls several additional functions that attempt to resolve a transformation
#'   from a `character` vector to a different type.  It is recommended for users
#'   to enter their own specifications instead.  Secondly, a `function` (with a
#'   single argument) can be passed which will then be applied directly to `x`.
#'   Third, a _prototype_ value can be passed.  This might be risky for special
#'   types.  Here, the values of [mode()], [storage.mode()], [attributes()], and
#'   [class()] are captured and reassigned from `to` to `x`.  A special check is
#'   implemented for `factor`s to more safely convert.  Lastly, `NULL` will do
#'   nothing and will simply return `x`.
#'
#' @examples
#' str(value_convert("2023-03-05", as.Date))
#' value_convert("a", factor(letters))
#' @returns A parsed value from `x`
#' @export
value_convert <- function(x, to = default_convert) {
  if (!is.character(x) || is.null(to)) {
    return(x)
  }

  if (is.factor(to)) {
    # special case for factors because they are annoying
    return(factor(x, levels = levels(to), ordered = is.ordered(to)))
  }

  if (is.function(to)) {
    to <- match.fun(to)
    return(to(x))
  }

  mode(x) <- mode(to)
  storage.mode(x) <- storage.mode(to)
  attributes(x) <- attributes(to)
  class(x) <- class(to)
  x
}

default_convert <- function(x) {
  if (!length(x)) {
    return(x)
  }

  out <- utils::type.convert(x, as.is = TRUE)

  # only handles defaults
  if (is.character(out)) {
    if (is_bool_like(out)) {
      out <- as_bool(out)
    } else {
      ok <- !is.na(out)
      dates <- suppressWarnings(as.POSIXct(x[ok], optional = TRUE))
      if (!anyNA(dates)) {
        out <- rep(as.POSIXct(NA), length(ok))
        out[ok] <- dates
      }
    }
  }

  out
}

as_bool <- function(x) {
  if (is.logical(x)) {
    return(x)
  }

  x <- trimws(x)
  out <- rep_len(NA, length(x))
  out[tolower(x) %in% bool_true()] <- TRUE
  out[tolower(x) %in% bool_false()] <- FALSE
  out
}

bool_values <- function() {
  c(bool_true(), bool_false(), "na", NA_character_)
}

bool_true <- function() {
  c("y", "yes", "t", "true", "1")
}

bool_false <- function() {
  c("n", "no", "f", "false", "0")
}

is_bool_like <- function(x) {
  if (is.logical(x)) {
    return(TRUE)
  }

  if (is.numeric(x)) {
    return(all(x %in% c(NA_integer_, 1L, 0L)))
  }

  if (!is.character(x)) {
    return(FALSE)
  }

  all(tolower(trimws(x)) %in% bool_values())
}
