context("Check loss estimation")
test_that("estLoss works as expected", {
    expect_identical(estLoss(0,2, "squared"), 4)
    expect_identical(estLoss(0,1, "binary"), 1)
    expect_identical(estLoss(0,0, "binary"), 0)
    expect_identical(estLoss(1,1, "binary"), 0)
})
