test_that("value_convert()", {
  obj <- as_bool(c("yes", "no", "TRUE", "1", "what", "2"))
  exp <- c(TRUE, FALSE, TRUE, TRUE, NA, NA)
  expect_identical(obj, exp)

  obj <- value_convert(c("yes", "no", "true", "false", "na"))
  exp <- c(TRUE, FALSE, TRUE, FALSE, NA)
  expect_identical(obj, exp)

  obj <- value_convert(c("2022-01-01", "NA"))
  exp <- as.POSIXct(c("2022-01-01", NA))
  expect_identical(obj, exp)

  expect_identical(default_convert(1.0), 1L)
  expect_identical(default_convert("1"), 1L)
  expect_identical(default_convert(1.2), 1.2)
  expect_identical(default_convert("1.2"), 1.2)
})

test_that("value_convert(x, factor)", {
  foo <- function(x = character()) {
    factor(x, levels = c("c", "a", "b"), ordered = TRUE)
  }
  obj <- value_convert(c("a", "b", "c"), foo())
  exp <- foo(c("a", "b", "c"))
  expect_identical(obj, exp)
})

test_that("value_convert(x, function)", {
  foo <- function(x) {
    as.integer(x) - 1L
  }
  x <- c("0", "1", "1.1", "a")
  obj <- suppressWarnings(value_convert(x, foo))
  exp <- c(-1L, 0L, 0L, NA_integer_)
  expect_identical(obj, exp)
})
