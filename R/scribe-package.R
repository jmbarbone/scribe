#' @keywords internal
#' @importFrom methods new
#' @details For a quick overview, see `vignette("scribe")`
"_PACKAGE"

## usethis namespace: start
## usethis namespace: end
NULL
op.scribe <- list( # nolint: object_name_linter.
  scribe.flag.no = TRUE,
  scribe.interactive = NULL
)

.onAttach <- function(libname, pkgname) {
  options(op.scribe[setdiff(names(op.scribe), names(options()))])
}
