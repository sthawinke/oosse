#' Gene expression and phenotypes of Brassica napus (rapeseed) plants
#'
#' RNA-sequencing data of genetically identical Brassica napus plants in autumn, with 5 phenotypes next spring, as published by \insertRef{DeMeyer2022}{oosse}.
#'
#' @format A list with two components Expr and Pheno
#' \describe{
#'   \item{Expr}{Matrix with Rlog values of 1000 most expressed genes}
#'   \item{Pheno}{Data frame with 5 phenotypes and x and y coordinates of the plants in the field}
#' }
#' @source \doi{10.1101/2022.10.21.513275}
#' @references
#'   \insertCite{DeMeyer2022}{oosse}
"Brassica"