withr::local_options(list(scribe.interactive = TRUE))

test_that("---version, ---help", {
  expect_output(command_args("---version")$parse())
  expect_output(command_args("---help")$parse())
})

test_that("errors", {
  expect_error(
    scribeSuperArg$new("foo"),
    regexp = "super args aliases must start with ---",
    fixed = TRUE
  )
})
