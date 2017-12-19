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
standardize_address <- function(address, key, type = "json") {
  
  # set up the url
  root <- "https://maps.googleapis.com/maps/api/geocode/"
  url <- paste0(root, type, "?address=", str_replace_all(address, " ", "+"), "&key=", key)
  
  # make the api call
  response <- getURL(url)
  if(type == "json") r <- fromJSON(response)
  return(r)
}