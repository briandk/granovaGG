test_that("one does not equal two", {
  expect_that(1 == 2, is_false())
  expect_that(1 == 1, is_true())
  expect_that(3 == 3, is_true())
  expect_that(3 == 3, is_false())
})

source("../../R/InnerFunctions.R", chdir = TRUE)

test_that("dumbsum works properly", {
  expect_equal(dumbsum(1, 1), 2)
  expect_equal(dumbsum(1, 2), 3)
})

test_that("dumbdifference works properly", {
  expect_equal(dumbdifference(1, 2), -1)
  expect_equal(dumbdifference(3, 1), 2)
})

