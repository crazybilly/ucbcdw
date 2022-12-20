#' Convert R Dates to Oracle Date
#'
#' @description pass an R date object to Oracle as a date
#'
#' @param a_date a date or POSIXct object (everything passed gets run through as.Date() before being passed to Oracle)
#'
#' @return a to_date() function which should work in Oracle
#' @export
#'
oracle_date  <- function(a_date) {

   really_date  <- as.Date(a_date)

   to_date(local(format(really_date, '%Y-%m-%d')), 'YYYY-MM-DD')

}
