test_that("is_intish() works", {
  expect_true(is_intish(1:1))
  expect_true(is_intish(1:10))
  expect_true(is_intish(NA))

  expect_false(is_intish(1.1))
  expect_false(is_intish(NULL))
  expect_false(is_intish("a"))
  expect_false(is_intish(Sys.Date()))
})

test_that("is_empty()", {
  expect_true(is_empty(""))
  expect_true(is_empty(NULL))
  expect_true(is_empty(integer()))
  expect_false(is_empty(is_empty))
  expect_false(is_empty(new_arg()))
})

test_that("prints", {
  expect_output(print_line("foo"))
  expect_output(print_line0("foo"))
  expect_output(print_line0("foo"))
  expect_output(print_scribe_version())
})
