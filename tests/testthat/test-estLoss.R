context("Check loss estimation")
test_that("estLoss works as expected", {
    expect_identical(estLoss(0,2, "squared"), 4)
    expect_identical(estLoss(0, 0.8, "binary"), TRUE)
    expect_identical(estLoss(0, 0.3, "binary"), FALSE)
    expect_identical(estLoss(1, .7, "binary"), FALSE)
    expect_identical(estLoss(1, .7, "logistic"), -log(.7))
})
