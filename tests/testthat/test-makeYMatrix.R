context("Check multivariate matrix building")
test_that("outcome matrix is built as expected", {
    expect_equal(makeYMatrix(factor(rep(0:2, times  = 3))), rbind(diag(3), diag(3), diag(3)))
})
