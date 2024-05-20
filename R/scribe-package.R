# nolint start: line_length_linter.

#' @keywords internal
#' @importFrom methods new
#' @details For a quick overview, see `vignette("scribe")`
#' @section options:
#' \describe{
#'   \item{`scribe.flag.no`}{`[TRUE]`\cr Default value when `scribeArg` uses action `"flag"`}
#'   \item{`scribe.interactive`}{`[NULL]`\cr Controls `interactive()` check within internal `exit()`}
#' }
"_PACKAGE"

## usethis namespace: start
## usethis namespace: end
NULL

# nolint end: line_length_linter.
# nolint next: object_name_linter.
op.scribe <- list(
  scribe.flag.no = TRUE,
  scribe.interactive = NULL,
  scribe.include = c("help", "version")
)

.onAttach <- function(libname, pkgname) {
  options(op.scribe[setdiff(names(op.scribe), names(options()))])
}
