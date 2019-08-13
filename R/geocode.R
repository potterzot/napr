#' Standardize an address using google maps api.
#' 
#' @import RCurl
#' @import stringr
#' @import jsonlite
#' @export
#' 
#' @param address (string) an address string
#' @param key (string) API key.
#' @param type (string) type of returned data. 
#' 
#' @return address components in the specified format.
geocode_google <- function(address, key, type = "json") {
  
  # set up the url
  root <- "https://maps.googleapis.com/maps/api/geocode/"
  url <- paste0(root, type, "?address=", str_replace_all(address, " ", "+"), "&key=", key)
  
  # make the api call
  response <- getURL(url)
  if(type == "json") r <- fromJSON(response)
  return(r)
}

#' Return centroid of an address or area using census geocode.
#' 
#' Uses Census geocode service to return the location of a given address. Based
#' on the Census API. Documentation and information is available here: 
#' \url{https://www.census.gov/geo/maps-data/data/geocoder.html}.
#' 
#' @import RCurl
#' @import stringr
#' @import jsonlite
#' @import httr
#' @export
#' 
#' @param address (string) address as a string (if searchtype = 
#'   "onelineaddress"), as a list of address components (if searchtype = 
#'   "address"), or as a list of x,y coordinates (if searchtype = 
#'   "coordinates").
#' @param key (string) API key.
#' @param returntype whether to include just locations or also geographies.
#' @param searchtype format of address to search on.
#' @param benchmark data benchmark to use in geocoding.
#' @param format format of returned data.
#' 
#' @return latitude and longitude of centroid.
geocode_census <- function(address, 
  returntype = c("locations", "geographies"),
  searchtype = c("onelineaddress", "address", "coordinates"),
  benchmark = c("Current", "ACS2017", "Census2010"),
  format = c("json", "html")) {
  
  returntype <- match.arg(returntype)
  searchtype <- match.arg(searchtype)
  benchmark  <- paste0("Public_AR_", match.arg(benchmark))
  format <- match.arg(format)
  
  # Build the parameter string
  params <- list(
    "address" = str_replace_all(str_replace_all(address, " ", "+"), ",", "%2C"),
    "benchmark" = benchmark,
    "format" = format
  )
  
  param_string <- paste0(
    sapply(names(params), function(n) { paste(n, params[n], sep="=") }), 
    collapse="&")
  
  # Build the URL
  url_root <- paste0("https://geocoding.geo.census.gov/geocoder/", 
                  returntype, "/", searchtype, "?")
  url <- paste0(url_root, param_string)
  
  # make the api call
  response <- getURL(url)
  r <- if(format == "json") try(fromJSON(response)) else response
  
  if(class(r) == "try-error") {
    message(paste0("Address: ", address, " returned an error, returning error message."))
  }
  r
}