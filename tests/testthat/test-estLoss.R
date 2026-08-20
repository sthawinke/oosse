context("Check loss estimation")
test_that("estLoss works as expected", {
  expect_identical(estLoss(0, 2, "squared"), 4)
  expect_true(estLoss(0, 0.8, "binary"))
  expect_false(estLoss(0, 0.3, "binary"))
  expect_false(estLoss(1, .7, "binary"))
  expect_identical(estLoss(1, .7, "logistic"), -log(.7))
  expect_identical(estLoss(0, .7, "logistic"), -log(1 - .7))
})
