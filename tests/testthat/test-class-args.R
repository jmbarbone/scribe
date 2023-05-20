withr::local_options(list(scribe.interactive = TRUE))

test_that("scribeArgs", {
  x <- new_arg()
  expect_true(is_arg(x))

  obj <- new_arg(action = "dots")$get_aliases()
  exp <- "..."
  expect_identical(obj, exp)
})

test_that("errors", {
  expect_error(new_arg(action = "list", n = 0))
  expect_error(new_arg(action = "flag", n = 1))
  expect_error(new_arg(c("foo", "--bar")))

  ca <- command_args(string = "-a 2 -a 1")
  ca$add_argument("-a")
  expect_warning(expect_warning(ca$parse()))
})

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
  op <- options(scribe.interactive = TRUE)
  obj <- new_arg("...", info = "help text")$get_help()
  exp <- c("...", "help text")
  expect_identical(obj, exp)

  obj <- new_arg(
    c("...", "dots"),
    info = "more help here"
  )$get_help()
  exp <- c("...", "dots: more help here")
  expect_identical(obj, exp)

  obj <- new_arg("-f", action = "flag", info = "help me")$get_help()
  exp <- c("-f", "help me")
  expect_identical(obj, exp)

  obj <- new_arg(c("-f", "--force"), action = "flag", info = "ugh")$get_help()
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
  options(op)
})

test_that("new_arg(action = 'default')", {
  expect_identical(new_arg()$action, "list")
  expect_identical(new_arg(options = list(choices = 1:2))$action, "list")
  expect_identical(new_arg(n = 0)$action, "flag")
  expect_identical(new_arg(options = list(no = FALSE))$action, "flag")
})

test_that("action = 'flag' allows TRUE [#55]", {
  ca <- command_args()
  expect_warning(
    ca$add_argument("--foo", action = "flag", default = TRUE),
    NA
  )
  obj <- ca$parse()
  exp <- list(foo = TRUE)
  expect_identical(obj, exp)

  ca$set_input("--foo")
  obj <- ca$parse()
  exp <- list(foo = TRUE)
  expect_identical(obj, exp)

  ca$set_input("--no-foo")
  obj <- ca$parse()
  exp <- list(foo = FALSE)
  expect_identical(obj, exp)
})

test_that("pass arg as default [#54]", {
  arg <- new_arg("-a")
  new <- new_arg("-b", default = arg)
  expect_output(expect_warning(print(new), "has not been resolved"))
})

test_that("length(info) > 1 [#57]", {
  ca <- command_args("--help", include = c("help", "version"))
  ca$add_argument("bad", info = c("one", "two"))
  expect_length(ca$get_args()[[3]]$get_help(), 2)
  expect_output(ca$parse())
})

test_that("snapshots", {
  arg <- new_arg("...", info = "help text")
  expect_output(arg$show())
  expect_output(arg$help())
  expect_snapshot(arg$show())
  expect_snapshot(arg$help())
})
