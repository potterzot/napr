#' @title napr-package.
#'
#' @description Functions and other bits of R code for frequent use.
#'
#' @author Nicholas Potter
#' @docType package
#' @name napr
#' @aliases napr
#'
NULL

#' Calculate distance between lat/long pairs.
#'
#' @param lat1 a list of lat/long values for location 1
#' @param lat2 a list of lat/long values for location 2
#' @param lon1 a list of lat/long values for location 1
#' @param lon2 a list of lat/long values for location 2
#' @return distance from loc1 to loc2.
distance <- function(lat1, lat2, lon1, lon2) {
  #From http://williams.best.vwh.net/avform.htm#Intro
  #d=2*asin(sqrt((sin((lat1-lat2)/2))^2 + cos(lat1)*cos(lat2)*(sin((lon1-lon2)/2))^2))
  2*asin(sqrt((sin((lat1-lat2)/2))^2 + cos(lat1)*cos(lat2)*(sin((lon1-lon2)/2))^2))
}

#' Tests if year is a leap year.
#'
#' @export
#' @param year an integer
#' @return boolean
#' @examples
#' is.leapYear(2000) #TRUE
#' is.leapYear(2003) #FALSE
is.leapYear <- function(yr) {
  as.logical(!(yr %% 400) || !((yr %% 4) && (yr %% 100)))
}

#' Rounds to arbitrary number.
#'
#' @export
#' @param n number to round.
#' @param r value to round to.
#' @return numeric.
#' @examples
#' round_any(1.32, .5)
#' #> 1.5
round_any <- function(n, r) round(n/r)*r

#' Fast %in% operator using fastmatch.
#'
#' @importFrom fastmatch fmatch
#' @export
#' @param x a value.
#' @param table the lookup table.
#' @return a logical.
`%fin%` <- function(x, table) {
  if (!requireNamespace("fastmatch", quietly = TRUE)) {
    stop("Please install fastmatch.", call. = FALSE)
  }
  fmatch(x, table, nomatch = 0L) > 0L
}
