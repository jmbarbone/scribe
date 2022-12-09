test_that("$get_names() [#10]", {
  x <- new_arg("-f", n = 1L)
  expect_identical(x$get_name(), "f")
  expect_identical(x$get_name(FALSE), "-f")

  x <- new_arg(c("-f", "--foo"), n = 1L)
  expect_identical(x$get_name(), "foo")
  expect_identical(x$get_name(FALSE), "--foo")

  x <- new_arg("foo", n = 1L)
  expect_identical(x$get_name(), "foo")

  x <- new_arg(c("...", "foo"), n = NA_integer_)
  expect_identical(x$get_name(), "foo")

  x <- new_arg("...", n = NA_integer_)
  expect_identical(x$get_name(), "...")
})

test_that("new_arg() defaults [#9]", {
  expect_error(new_arg(), NA)
  expect_error(new_arg("..."), NA)
  expect_error(new_arg(action = "dots"), NA)
  expect_error(new_arg(action = "flag"), NA)
})
