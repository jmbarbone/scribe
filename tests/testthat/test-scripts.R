path <- test_path("scripts/help.R")
path <- normalizePath(path)
Sys.chmod(path)

setup_scripts <- function(frame = parent.frame()) {
  testthat::skip_if(fuj::is_windows())
  testthat::skip_on_cran()
  setup_scribe(frame)
}

test_that("scribe --help works", {
  setup_scripts()
  obj <- rscript(path, "--help", .env = r_libs_scribe_test())[-3L]
  exp <- c(
    "{scribe} command_args",
    "",
    "file :"[0],
    "",
    "USAGE",
    "  help.R [--help | --version]",
    "  help.R [-f, --foo [ARG]] [-b, --bar [ARG]]",
    "",
    "ARGUMENTS",
    "  --help          prints this and quietly exits                   ",
    "  --version       prints the version of {scribe} and quietly exits",
    "  -f, --foo [ARG]                                                 ",
    "  -b, --bar [ARG]                                                 ",
    NULL
  )
  expect_identical(trimws(obj), trimws(exp))
})

test_that("scribe ---help works", {
  setup_scripts()
  obj <- rscript(path, "---help", .env = r_libs_scribe_test())
  exp <- c(
    paste0("{scribe} v", format(scribe_version(dev = TRUE))),
    "For more information, see https://jmbarbone.github.io/scribe/"
  )
  expect_identical(trimws(obj), trimws(exp))
})

test_that("scribe ---version works", {
  setup_scripts()
  obj <- rscript(path, "---version", .env = r_libs_scribe_test())
  expect_identical(obj, format(scribe_version(dev = TRUE)))
})
