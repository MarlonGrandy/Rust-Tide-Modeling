#' Convert to as POSIXct to an 8D week number (1-46) or 7D week number (1-52)
#'
#' @export
#' @param x POSIXct or Date vector
#' @param week_length numeric, the number of days per week
#' @return numeric 1-46 or 1-52 week number
date_to_week <- function(x, week_length = c(8,7)[2]){
  if (missing(x)) x <- Sys.Date()
  J <- as.numeric(format(x, "%j"))
  (J-1) %/% week_length + 1
}
