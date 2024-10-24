#' Estimate the loss between observed and predicted values
#'
#' @param obs,pred Observed and predicted observations
#' @param loss The loss type, see details
#' @details The loss function quantifies discrepancy between observed and predicted values.
#' The lower the loss, the better the prediction. The output for "squared" is (obs-pred)^2, for
#' "binary" it is 1-I(obs=pred), for "logistic" it is -(obs*log(pred)+(1-obs)log(1-pred))
#'
#' @return A vector of losses of the same length as obs and pred
estLoss = function(obs, pred, loss){
    switch(loss,
           "squared" = (obs-pred)^2,
           "binary" = obs != round(pred),
           "logistic" = -(obs*log(pred) + (1-obs)*log(1-pred)))
}