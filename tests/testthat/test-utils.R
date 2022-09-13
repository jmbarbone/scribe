test_that("is_intish() works", {
  expect_true(is_intish(1:1))
  expect_true(is_intish(1:10))
  expect_true(is_intish(NA))

  expect_false(is_intish(1.1))
  expect_false(is_intish(NULL))
  expect_false(is_intish("a"))
  expect_false(is_intish(Sys.Date()))
})
