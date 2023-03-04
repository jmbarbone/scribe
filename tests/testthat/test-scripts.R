path <- test_path("scripts/help.R")
path <- normalizePath(path)
Sys.chmod(path)

test_that("help works", {
  skip_on_cran()
  skip_on_ci()
  # lazy testing
  obj <- system2(path, "--help", stdout = TRUE)[-3]
  exp <- c(
    "{scribe} command_args",
    "",
    "file :"[0],
    "",
    "USAGE",
    "  help.R [--help | --version]",
    "  help.R [-f, --foo [ARG]] [-b, --bar [ARG]] ",
    "",
    "ARGUMENTS",
    "  --help          : prints this and quietly exits                   ",
    "  --version       : prints the version of {scribe} and quietly exits",
    "  -f, --foo [ARG] :                                                 ",
    "  -b, --bar [ARG] :                                                 ",
    NULL
  )
  expect_identical(obj, exp)
})

test_that("help works", {
  skip_on_cran()
  skip_on_ci()
  obj <- system2(path, "--version", stdout = TRUE)
  exp <- utils::capture.output(print_scribe_version())
  expect_identical(obj, exp)
})
