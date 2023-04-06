#' Format seconds into human readable format
#'
#' @param seconds The number of seconds to be formatted
#' @param digits the number of digits for rounding
#'
#' @return A character vector expressing time in human readable format
formatSeconds = function(seconds, digits = 2){
    minutes = seconds %/% 60
    hours = minutes %/% 60
    days = hours %/% 24
    paste0(if(days > 0) {paste(round(days, digits), "days, ")},
           if(hours > 0) {paste(round(hours%%24, digits), "hours, ")},
           if(minutes > 0) {paste(round(minutes%%60, digits), "minutes and ")},
           paste(round(seconds %% 60, digits), "seconds"))
}