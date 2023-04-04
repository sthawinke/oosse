#' Format seconds into human readable format
#'
#' @param seconds
#'
#' @return A character vector expressing time in human readable format
formatSeconds = function(seconds){
    minutes = seconds %/% 60
    hours = minutes %/% 60
    days = hours %/% 24
    paste0(if(days > 0) {paste(days, "days, ")},
           if(hours > 0) {paste(hours%%24, "hours, ")},
           if(minutes > 0) {paste(minutes%%60, "minutes and ")},
           paste(seconds %% 60, "seconds"))
}