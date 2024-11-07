context("Check multivariate matrix building")
test_that("outcome matrix is built as expected", {
    tmp = makeYMatrix(factor(rep(0:2, times  = 3)))
    attributes(tmp) = NULL
    attr(tmp, "dim") = c(9,3)
    expect_equal(tmp, rbind(diag(3), diag(3), diag(3)))
})
