#' Makes a call to ecotopia API to download the indicated data.
#'
#' @description ecot.API.request makes an unique request to Ecotopia API based on the choosen type of data, individual and date (optional) for a defined number (optional but always <= 1000) of resulting data.
#'
#' @param token a character string giving your token for an open session. For obtaining it, see ecot.token.
#' @param indv_id a character string of the device id from the table of devices information obtained with a call to the API. For obtaining it, see ecot.indvs. The 3 different identifiers (uuid, device number, and device id) that appears on the Ecotopia webpage are not useful for this.
#' @param ndevicelimit the maximun number of data to download on each request to the API. The maximun available is 1000, which is the default value.
#' @param datestart_updates a character string giving a date on the format "Y%-%m-%d %H:%M:$S" indicating date from which start the downloading. If this is not provided the downloads will start at first date according to "Deployment date" on Ecotopia webpage.
#' @param type a character string indicating the kind of data to download. It can be "GPS", "Env", "ODBA" or "Acc"
#'
#' @return a list of data frame. Each element of the list is the downloadED information for each individual.
#' @export
#'
#' @examples ecot.downloads(ecot.token("abc","passw"), "6267d0fae75c8ef26173d757", type = "Env")
#'
ecot.API.request <- function(token, indv_id, ndevicelimit = 1e3, datestart_updates = NA, type = "GPS"){

  list.of.packages <- c("httr", "jsonlite", "rjson") ## needed packages
  new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
  if (length(new.packages)) install.packages(new.packages)
  invisible(lapply(list.of.packages, library, character.only = TRUE))

  if(type == "GPS")
    tourl <- "gps"
  if(type == "Env")
    tourl <- "env"
  if(type == "ODBA")
    tourl <- "behavior2"
  if(type == "Acc")
    tourl <- "origin"


  if(is.na(datestart_updates))
    url <- paste0("https://ecotopiago.com/api/v2/",tourl,"/device/",indv_id,"/page") else
      url <- paste0("https://ecotopiago.com/api/v2/",tourl,"/device/",indv_id,"/page/",datestart_updates)


  headers <-
    add_headers("x-druid-authentication" = token, "x-result-limit" = ndevicelimit, "x-result-sort" = "timestamp") #el header de orden ahce falta para que no descargue desde el ultimo hacia el final

  input <- GET(url, config = headers)
  input <- content(input, as = "parsed")

  return(input)
}
