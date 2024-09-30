#' Estimate the loss between observed and predicted values
#'
#' @param obs,pred Observed and predicted observations
#' @param loss The loss type. For "squared" this is (obs-pred)^2, for
#' "binary" it is I(obs=pred).
#'
#' @return A vector of losses of the same length as obs and pred
estLoss = function(obs, pred, loss){
    switch(loss,
           "squared" = (obs-pred)^2,
           "binary" = obs == pred)
}