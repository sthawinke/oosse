#' Helper function to check if matrix is positive definite
#'
#' @param mat The matrix
#' @param tol The tolerance
#'
#' @return A boolean indicating positive definiteness
isPD = function(mat, tol = 1e-6){
    ev = eigen(mat, symmetric = TRUE)$values
    all(ev >= -tol * abs(ev[1L]))
}