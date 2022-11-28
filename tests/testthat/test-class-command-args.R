test_that("command_args() works", {
  x <- c("-a", "1", "-b", "2")
  ca <- command_args(x)
  ca$add_argument("-a", "--alpha", type = "integer")
  ca$add_argument("-b", "--beta", type = "integer")
  # debugonce(ca_resolve_args)
  # debug(arg_parse_value)
  ca$resolve()
  ca$get_values()

  obj <- ca$parse()
  exp <- list(alpha = 1L, beta = 2L)
  expect_identical(obj, exp)
})
