test_that("command_args() works", {
  x <- c("-a", "1", "-b", "2")
  ca <- command_args(x)
  ca$add_argument("-a", "--alpha")
  ca$add_argument("-b", "--beta")

  obj <- ca$parse()
  exp <- list(alpha = 1L, beta = 2L)
  expect_identical(obj, exp)
})

test_that("command_args() handles defaults", {
  foo <- function(x = character()) {
    ca <- command_args(x)
    ca$add_argument("-a", "--alpha", default = 1L)
    ca$add_argument("-b", "--beta",  default = 1L)
    ca$parse()
  }

  obj <- foo()
  exp <- list(alpha = 1L, beta = 1L)
  expect_identical(obj, exp)

  obj <- foo(c("-a", "0"))
  exp <- list(alpha = 0L, beta = 1L)
  expect_identical(obj, exp)

  obj <- foo(c("-a", "2", "--beta", "3"))
  exp <- list(alpha = 2L, beta = 3L)
  expect_identical(obj, exp)
})

test_that("command_args() handles dots", {
  x <- c("-a", "1", "2", "3")
  ca <- command_args(x)
  ca$add_argument("-a", "--alpha")
  ca$add_argument("...", "values")
  obj <- ca$parse()
  exp <- list(alpha = 1L, values = 2:3)
  expect_identical(obj, exp)
})

test_that("bad arguments don't create NULLs", {
  ca <- command_args()
  expect_error(ca$add_argument("-a", convert = as.integer, default = "1"))
  expect_identical(ca$nArgs, 0L)
  expect_identical(ca$argList, list())
})

test_that("$add_argument('...', default = character()) [#3]", {
  obj <- command_args("foo")$add_argument("...")$parse()
  exp <- list(... = "foo")
  expect_identical(obj, exp)
})

test_that("$add_argument('...', default = 'bar') [#11]", {
  obj <- command_args("")$add_argument("...", default = "bar")$parse()
  exp <- list(... = "bar")
  expect_identical(obj, exp)

  obj <- command_args("")$add_argument("...", default = 1:3)$parse()
  exp <- list(... = 1:3)
  expect_identical(obj, exp)
})

test_that("$add_argument(action = 'flag') [#17]", {
  obj <- command_args()$add_argument("-f", "--foo", action = "flag")$parse()
  exp <- list(foo = FALSE)
  expect_identical(obj, exp)

  obj <- command_args("-f")$add_argument("-f", "--foo", action = "flag")$parse()
  exp <- list(foo = TRUE)
  expect_identical(obj, exp)

  obj <- command_args("--foo")$add_argument("-f", "--foo", action = "flag")$parse() # nolint: line_length_linter.
  exp <- list(foo = TRUE)
  expect_identical(obj, exp)

  # nolint start: line_length_linter.
  expect_warning(command_args()$add_argument("f", action = "flag", default = TRUE))
  expect_warning(command_args()$add_argument("f", action = "flag", default = "1"))
  expect_warning(command_args()$add_argument("f", action = "flag", default = "1"))
  # nolint end: line_length_linter.
})

test_that("adding args is fine", {
  # should be fine during creation
  ca <- command_args(c("foo", "bar"))
  ca$add_argument("foo", action = "flag")
  ca$add_argument("bar", action = "flag")

  obj <- ca$get_input()
  exp <- c("foo", "bar")
  expect_identical(obj, exp)

  obj <- ca$parse()
  exp <- list(foo = TRUE, bar = TRUE)
  expect_identical(obj, exp)
})

test_that("adding args is fine after initialization [#19]", {
  # should be okay after ca is created
  ca <- command_args()
  obj <- ca$get_input()
  exp <- character()
  expect_identical(obj, exp)

  ca$set_input(c("foo", "bar"))
  obj <- ca$get_input()
  exp <- c("foo", "bar")
  expect_identical(obj, exp)

  ca$add_argument("foo", action = "flag")
  ca$add_argument("bar", action = "flag")
  obj <- ca$parse()
  exp <- list(foo = TRUE, bar = TRUE)
  expect_identical(obj, exp)
})

test_that("args are returned in original order [#25]", {
  ca <- command_args(c("-b", "one", "-c", "two", "-a", "three", "foo", "bar"))
  ca$add_argument("...")
  ca$add_argument("-a", default = "zero")
  ca$add_argument("-c", default = "zero")
  ca$add_argument("-b", default = "zero")
  obj <- ca$parse()
  exp <- list(... = c("foo", "bar"), a = "three", c = "two", b = "one")
  expect_identical(obj, exp)
})
