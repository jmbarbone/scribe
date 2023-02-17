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

test_that("positional() [#22]", {
  expect_true(new_arg("foo")$positional)
  expect_false(new_arg("--foo")$positional)
})

test_that("help() [#16]", {
  obj <- new_arg("...", help = "help text")$get_help()
  exp <- c("...", "help text")
  expect_identical(obj, exp)

  obj <- new_arg(
    c("...", "dots"),
    help = "more help here"
  )$get_help()
  exp <- c("...", "dots: more help here")
  expect_identical(obj, exp)

  obj <- new_arg("-f", action = "flag", help = "help me")$get_help()
  exp <- c("-f", "help me")
  expect_identical(obj, exp)

  obj <- new_arg(c("-f", "--force"), action = "flag", help = "ugh")$get_help()
  exp <- c("-f, --force, --no-force", "ugh")
  expect_identical(obj, exp)

  obj <- new_arg("--values", action = "list")$get_help()
  exp <- c("--values [ARG]", "")
  expect_identical(obj, exp)

  obj <- new_arg(
    c("-v", "--values"),
    action = "list",
    options = list(choices = c("one", "two", "three"))
  )$get_help()
  exp <- c("-v, --values [ARG]", "(one, two, three)")
  expect_identical(obj, exp)
})
