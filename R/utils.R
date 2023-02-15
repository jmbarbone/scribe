
# slightly different here

`%||%` <- function(x, y) { if (is_empty(x)) y else x } # nolint: brace_linter.

is_empty <- function(x) {
  if (is.function(x)) {
    FALSE
  } else {
    is.null(x) || length(x) == 0L || !any(nzchar(x))
  }
}

to_string <- function(..., sep = " ") {
  lines <- list(...)
  lines <- unlist(lines)
  paste0(lines, collapse = sep)
}

print_lines <- function(..., sep = "\n", to = stdout()) {
  writeLines(to_string(..., sep = sep), con = to)
}

print_line <- function(...) {
  print_lines(..., sep = " ")
}

print_line0 <- function(...) {
  print_lines(..., sep = "")
}

is_intish <- function(x) {
  !is.null(x) && (is.numeric(x) | all(is.na(x))) && !isFALSE(x %% 1 == 0)
}

maybe_quit <- function(force = !interactive()) {
  if (force) quit(save = "no", status = 0L) else invisible()
}

first <- function(x) {
  if (length(x)) x[1L] else x[0L]
}

# nolint start: object_name_linter.
wapply <- function(x, FUN, ...) {
  FUN <- match.fun(FUN)
  # nolint end: object_name_linter.
  fun <- function(x, ...) isTRUE(FUN(x, ...))
  which(do.call(vapply, list(X = x, FUN = fun, FUN.VALUE = NA)))
}

exit <- function(
    # allow for manual checking
    force = !getOption("scribe.interactive", interactive())
  ) {

  # nocov start
  if (force) {
    # testing in tests/testthat/scripts/help.R
    quit()
  }
  # nocov end

  invisible()
}

print_scribe_version <- function() {
  print_line(
    "{scribe} package version:",
    format(utils::packageVersion("scribe"))
  )
}
