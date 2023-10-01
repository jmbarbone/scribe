withr::local_options(list(scribe.interactive = TRUE))

test_that("command_args() works", {
  x <- c("-a", "1", "-b", "2")
  ca <- command_args(x)
  ca$add_argument("-a", "--alpha")
  ca$add_argument("-b", "--beta")

  obj <- ca$parse()
  exp <- list(alpha = 1L, beta = 2L)
  expect_identical(obj, exp)

  expect_error(
    command_args("-i", string = "-i"),
    "cannot both be set"
  )

  expect_true(is_command_args(ca))
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
  ca <- command_args(include = NA)
  expect_error(ca$add_argument("-a", convert = as.integer, default = "1"))
  expect_identical(length(ca$args), 0L)
  expect_identical(ca$args, list())
})

test_that("resolving", {
  ca <- command_args(string = "--foo a")
  ca$add_argument("--foo", default = 0)
  expect_warning(ca$resolve())

  ca <- command_args(string = "--foo a")
  ca$add_argument("--fizz")
  expect_warning(ca$resolve(), "Not all values parsed")
  expect_true(ca$resolved)
  expect_identical(ca, ca$resolve())

  ca <- command_args("1")
  ca$add_argument("foo", convert = function(x) stop("my error"))
  w <- ca_get_working(ca)
  expect_error(ca$resolve(), "my error")
  expect_false(ca$resolved)
  expect_identical(w, ca_get_working(ca))
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
  ca <- command_args()
  ca$add_argument("-f", "--foo", action = "flag")
  obj <- ca$parse()
  exp <- list(foo = FALSE)
  expect_identical(obj, exp)

  obj <- ca$set_input("-f")$parse()
  exp <- list(foo = TRUE)
  expect_identical(obj, exp)

  obj <- ca$set_input("--foo")$parse()
  exp <- list(foo = TRUE)
  expect_identical(obj, exp)

  expect_error(command_args()$add_argument("f", action = "flag", default = "1"))
  expect_error(command_args()$add_argument("f", action = "flag", default = "1"))
})

test_that("$add_argument()", {
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

test_that("$add_argument(named list)", {
  ca <- command_args()
  expect_warning(
    ca$add_argument("foo", bad_name = "bar"),
    "names: bad_name"
  )
})

test_that("$add_argument() after initialization [#19]", {
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

test_that("$add_argument(arg) [#45]", {
  ca <- command_args(include = NA)
  arg <- new_arg("foo")
  ca$add_argument(arg)
  obj <- ca$get_args()
  exp <- list(arg)
  expect_identical(obj, exp)
})

test_that("args are returned in original order [#25]", {
  ca <- command_args(
    c("-b", "one", "-c", "two", "-a", "three", "foo", "bar"),

  )
  ca$add_argument("...")
  ca$add_argument("-a", default = "zero")
  ca$add_argument("-c", default = "zero")
  ca$add_argument("-b", default = "zero")
  obj <- ca$parse()
  exp <- list(... = c("foo", "bar"), a = "three", c = "two", b = "one")
  expect_identical(obj, exp)
})

test_that("default values", {
  ca <- command_args(c("1", "2.0", "3", "4.0"))
  ca$add_argument("one",   default = 0L)
  ca$add_argument("two",   default = 0)
  ca$add_argument("three", default = 0)
  ca$add_argument("four",  default = 0L)
  obj <- ca$parse()
  exp <- list(
    one   = 1L,
    two   = 2,
    three = 3,
    four  = 4L
  )
  expect_identical(obj, exp)
})

test_that("$get_args(included)", {
  ca <- command_args()
  ca$add_argument("foo")
  ca$add_argument("biz")

  obj <- sapply(ca$get_args(included = TRUE), function(arg) arg$get_name())
  exp <- c("help", "version", "foo", "biz")
  expect_identical(obj, exp)

  obj <- sapply(ca$get_args(included = FALSE), function(arg) arg$get_name())
  exp <- c("foo", "biz")
  expect_identical(obj, exp)
})

test_that("positional values [#22]", {
  ca <- command_args(1:2)
  ca$add_argument("foo", default = 0)
  ca$add_argument("bar", default = 0)
  obj <- ca$parse()
  exp <- list(foo = 1, bar = 2)
  expect_identical(obj, exp)

  ca <- command_args(c(1, "--bar", 2))
  ca$add_argument("foo", default = 0)
  ca$add_argument("--bar", default = 0)
  obj <- ca$parse()
  exp <- list(foo = 1, bar = 2)
  expect_identical(obj, exp)

  ca <- command_args(c("--bar", 2, 1))
  ca$add_argument("foo", default = 0)
  ca$add_argument("--bar", default = 0)
  obj <- ca$parse()
  exp <- list(foo = 1, bar = 2)
  expect_identical(obj, exp)
})

test_that("n args [#20]", {
  ca <- command_args(c("--values", "1", "2"))
  ca$add_argument("--values", n = 2)
  obj <- ca$parse()
  exp <- list(values = 1:2)
  expect_identical(obj, exp)

  ca <- command_args(c("--values", 1:5))
  ca$add_argument("--values", n = 3)
  ca$add_argument("...")
  obj <- ca$parse()
  exp <- list(values = 1:3, `...` = 4:5)
  expect_identical(obj, exp)
})

test_that("string input [#24]", {
  ca <- command_args(string = "one two 'three four'")
  obj <- ca$input
  exp <- c("one", "two", "three four")
  expect_identical(obj, exp)

  ca <- command_args(string = 'one two "three four"')
  obj <- ca$input
  expect_identical(obj, exp)
})

test_that("--help has early stop", {
  ca <- command_args("--help")
  ca$add_argument("-v")
  ca$add_argument("-f")
  exp <- list(help = TRUE)
  expect_output(obj <- try(ca$parse()))
  expect_identical(obj, exp)
})

test_that("- parsed as _ [#33]", {
  ca <- command_args("--foo-bar")
  ca$add_argument("--foo-bar")
  obj <- ca$parse()
  exp <- list(foo_bar = NA)
  expect_identical(obj, exp)
})

test_that("--no-flag parses [#34]", {
  ca <- command_args("--no-foo")
  ca$add_argument("-f", "--foo", action = "flag")
  obj <- ca$parse()
  exp <- list(foo = FALSE)
  expect_identical(obj, exp)
})

test_that("descriptions", {
  ca <- command_args()
  ca$add_description("First part here.", "  Followed by a second sentences.")
  expect_output(ca$help())
  ca$add_description("A new line should be appended.")
  expect_output(ca$help())
  ca$set_description("description")
  expect_output(ca$help())
  ca$set_description()
  expect_output(ca$help())
})

test_that("descriptions snaps", {
  ca <- command_args()
  ca$add_description("First part here.", "  Followed by a second sentences.")
  expect_snapshot(ca$help())
  ca$add_description("A new line should be appended.")
  expect_snapshot(ca$help())
  ca$set_description("description")
  expect_snapshot(ca$help())
  ca$set_description()
  expect_snapshot(ca$help())
})

test_that("examples [#38]", {
  ca <- command_args()
  ca$add_example("foo --flag")
  ca$add_example(NULL)
  ca$add_example("foo --other-flag")
  expect_output(ca$help())
  ca$set_example()
  expect_output(ca$help())
  ca$set_example("foo command value")
  expect_output(ca$help())
})

test_that("examples snaps", {
  ca <- command_args()
  ca$add_example("foo --flag")
  ca$add_example(NULL)
  ca$add_example("foo --other-flag")
  expect_snapshot(ca$help())
  ca$set_example()
  expect_snapshot(ca$help())
  ca$set_example("foo command value")
  expect_snapshot(ca$help())
})

test_that("versions", {
  ca <- command_args(string = "--version", include = "version")
  ca$add_argument("--foo")
  ca$add_argument("--bar")
  ca$add_description("This does things")
  expect_output((obj <- ca$parse()))
  exp <- list(version = TRUE)
  expect_identical(obj, exp)
  expect_output(ca$version())
})

test_that("positional defaults [#52]", {
  ca <- command_args()
  ca$add_argument("pos", default = 1)
  obj <- ca$parse()
  exp <- list(pos = 1)
  expect_identical(obj, exp)
})

test_that("pass arg as default [#54]", {
  ca <- command_args()
  arg <- new_arg("-a", action = "flag")
  ca$add_argument(arg)
  ca$add_argument("-b", default = arg)

  obj <- ca$parse()
  exp <- list(a = FALSE, b = FALSE)
  expect_identical(obj, exp)

  ca$set_input("-a")
  obj <- ca$parse()
  exp <- list(a = TRUE, b = TRUE)
  expect_identical(obj, exp)

  ca$set_input("-b")
  obj <- ca$parse()
  exp <- list(a = FALSE, b = TRUE)
  expect_identical(obj, exp)

  ca$set_input(c("-a", "-b"))
  obj <- ca$parse()
  exp <- list(a = TRUE, b = TRUE)
  expect_identical(obj, exp)
})

test_that("'stop' args [#60]", {
  ca <- command_args(string = "-a -b")
  ca$add_argument("-a", action = "flag", stop = "hard")
  ca$add_argument("-b", action = "flag")
  obj <- ca$parse()
  exp <- list(a = TRUE)
  expect_identical(obj, exp)

  ca <- command_args(string = "-a -b")
  ca$add_argument("-a", action = "flag", stop = "soft")
  ca$add_argument("-b", action = "flag")
  obj <- ca$parse()
  exp <- list(a = TRUE, b = FALSE)
  expect_identical(obj, exp)

  expect_identical(new_arg(stop = NA)$stop, "soft")
  expect_identical(new_arg(stop = TRUE)$stop, "hard")
  expect_identical(new_arg(stop = FALSE)$stop, "none")
})

test_that("'convert' isn't ignored [#70]", {
  ca <- command_args("1")
  ca$add_argument("foo", convert = function(...) stop("success"))
  expect_error(ca$parse(), "success")

  ca <- command_args("1")
  ca$add_argument("foo", convert = function(...) stop("success"), default = 1)
  expect_error(ca$parse(), "success")
})

test_that("snapshots", {
  ca <- command_args(string = "foo bar --fizz")
  ca$add_description("this does a thing")
  expect_output(ca$show())
  expect_output(ca$help())
  expect_snapshot(ca$show())
  expect_snapshot(ca$help())
})
