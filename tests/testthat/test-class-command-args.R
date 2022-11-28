test_that("command_args() works", {
  x <- c("-a", "1", "-b", "2")
  ca <- command_args(x)
  ca$add_argument("-a", "--alpha", type = "integer")
  ca$add_argument("-b", "--beta", type = "integer")

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
  ca$add_argument("-a", "--alpha", type = "integer")
  ca$add_argument("...", "values", type = "integer")
  obj <- ca$parse()
  exp <- list(alpha = 1L, values = 2:3)
  expect_identical(obj, exp)
})
