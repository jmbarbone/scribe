test_that("execute works", {
  arg <- new_arg(
    "foo",
    default = 1L,
    action = "list",
    execute = function(arg, ...) {
      message("The value is: ", arg$get_value()^2)
    }
  )

  ca <- command_args()
  ca$add_argument(arg)

  # sanity check
  expect_failure(expect_identical(
    ca$get_args()[[1]]$execute,
    invisible
  ))

  expect_message(ca$parse(), "The value is: 1")

  ca$set_input("2")
  expect_message(ca$parse(), "The value is: 4")
})

test_that("execute formals work", {
  arg0 <- new_arg(
    "a",
    default = 1,
    execute = function() stopifnot(TRUE)
  )

  arg1 <- new_arg(
    "b",
    default = 1,
    execute = function(x) stopifnot(is_arg(x))
  )

  arg3 <- new_arg(
    "c",
    default = 1,
    execute = function(x, y, z) {
      stopifnot(
        is_arg(x),
        is_command_args(y),
        missing(z)
      )
    }
  )

  ca <- command_args()
  ca$add_argument(arg0)
  ca$add_argument(arg1)
  ca$add_argument(arg3)
  expect_error(ca$parse(), NA)
})
