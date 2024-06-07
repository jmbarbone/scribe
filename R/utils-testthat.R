#' Check method arguments
#'
#' @param object Object to check
#' @param prefix Prefix to use for method names
#' @param verbose Print out additional information
#' @keywords internal
#' @noRd
check_methods <- function(object, prefix, verbose = getOption("verbose")) {
  requireNamespace("testthat")

  for (method in object$methods()) {
    obj <- get0(method, object$def@refMethods, mode = "function")

    if (is.null(obj)) {
      next
    }

    exp <- get0(paste0(prefix, method), mode = "function")

    if (is.null(exp)) {
      if (verbose) {
        cat("Skipping method: ", method, "()\n", sep = "") # nocov
      }

      next
    }

    obj <- as.list(formals(obj))

    if (identical(obj, list())) {
      # exp will be a named list, but will be empty
      obj <- structure(list(), names = character())
    }

    exp <- as.list(formals(exp))[-1L]

    testthat::expect_identical(
      object = obj,
      expected = exp,
      info = paste0("method: ", method, "()"),
      ignore_srcref = TRUE
    )
  }
}
