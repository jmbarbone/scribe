test_that("convert()", {
  obj <- as_bool(c("yes", "no", "TRUE", "1", "what", "2"))
  exp <- c(TRUE, FALSE, TRUE, TRUE, NA, NA)
  expect_identical(obj, exp)

  obj <- convert(c("yes", "no", "true", "false", "na"))
  exp <- c(TRUE, FALSE, TRUE, FALSE, NA)
  expect_identical(obj, exp)

  obj <- convert(c("2022-01-01", "NA"))
  exp <- as.POSIXct(c("2022-01-01", NA))
  expect_identical(obj, exp)

  expect_identical(default(1.0), 1L)
  expect_identical(default("1"), 1L)
  expect_identical(default(1.2), 1.2)
  expect_identical(default("1.2"), 1.2)
})

test_that("convert(x, factor)", {
  obj <- convert(c("a", "b", "c"), factor(levels = c("c", "a", "b"), ordered = TRUE))
  exp <- factor(c("a", "b", "c"), levels = c("c", "a", "b"), ordered = TRUE)
  expect_identical(obj, exp)
})

test_that("convert(x, function)", {
  foo <- function(x) as.integer(x) - 1L
  x <- c("0", "1", "1.1", "a")
  obj <- suppressWarnings(convert(x, foo))
  exp <- c(-1L, 0L, 0L, NA_integer_)
  expect_identical(obj, exp)
})
