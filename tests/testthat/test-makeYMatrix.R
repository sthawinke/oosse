context("Check multivariate matrix building")
test_that("outcome matrix is built as expected", {
    expect_equal(makeYMatrix(rep(0:2, times  = 3)), cbind(diag(3), diag(3), diag(3)))
})
