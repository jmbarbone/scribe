library(testthat)

# from RConsortium/S7 tests

setup_scribe <- function(frame = parent.frame()) {
  r_libs_scribe <- tempfile("R_LIBS_SCRIBE_TEST_")
  dir.create(r_libs_scribe, recursive = TRUE)
  Sys.setenv(R_LIBS_SCRIBE_TEST = r_libs_scribe)
  defer(
    try(unlink(r_libs_scribe, recursive = TRUE, force = TRUE)),
    frame = frame
  )
  quick_install(scribe_path(), r_libs_scribe)
}

r_libs_scribe_test <- function() {
  path <- Sys.getenv(
    "R_LIBS_SCRIBE_TEST",
    Sys.getenv("R_LIBS_PATH", NA_character_)
  )
  stopifnot(dir.exists(path))
  c(R_LIBS_SCRIBE_TEST = path)
}

scribe_path <- function() {
  find.package("scribe")
}

quick_install <- function(package, lib, quiet = TRUE) {
  opts <- c(
    "--data-compress=none",
    "--no-byte-compile",
    "--no-data",
    "--no-demo",
    "--no-docs",
    "--no-help",
    "--no-html",
    "--no-libs",
    "--use-vanilla",
    NULL
  )

  for (p in package) {
    utils::install.packages(
      pkgs = p,
      lib = lib,
      repos = NULL,
      type = "source",
      quiet = quiet,
      verbose = FALSE,
      INSTALL_opts = paste(opts, collapse = " ")
    ) |>
      withCallingHandlers(
        simpleWarning = function(w) {
          stop(simpleError(conditionMessage(w), conditionCall(w)))
        }
      )
  }
}

local_install_and_attach <- function(path, lib, frame = parent.frame()) {
  quick_install(path, lib)
  package <- basename(path)
  library(package, lib.loc = lib, character.only = TRUE)
  defer(
    try(detach(paste0("package:", package), unload = TRUE), silent = TRUE),
    frame = frame
  )
  invisible(package)
}

# Lightweight equivalent of withr::defer()
defer <- function(expr, frame = parent.frame(), after = FALSE) {
  thunk <- as.call(list(function() expr))
  do.call(on.exit, list(thunk, TRUE, after), envir = frame)
}
