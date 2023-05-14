test_that("execute works", {
  arg <- new_arg(
    "foo",
    default = 1L,
    action = "list",
    execute = function(arg, ...) {
      message("The value is: ", arg$get_value()^2)
    }
  )

  ca <- command_args(include = NA)
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
