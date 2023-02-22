#' Download devices information
#'
#' @param token a character string giving your token for an open session. For obtaining it, see ecot.token.
#' @param ndevicelimit the maximun number of data to download on each request to the API. The maximun available is 1000, which is the default value.
#'
#' @return a data.frame with devices information available. The column "id" contains the useful device id for the API request.
#' @export
#'
#' @examples devices_info <- ecot.indvs(ecot.token("abc","passw"))
#'
ecot.indvs <- function(token, ndevicelimit = 1e3){

  list.of.packages <- c("httr", "jsonlite", "rjson") ## needed packages
  new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
  if (length(new.packages)) install.packages(new.packages)
  invisible(lapply(list.of.packages, library, character.only = TRUE))

  headers <-
    add_headers("x-druid-authentication" = token, "x-result-limit" = ndevicelimit)
  url <- "https://ecotopiago.com/api/v3/device/page/"
  input <- GET(url, config = headers)
  input <- content(input, as = "parsed")

  pull <- lapply(input, ecot.unlist.JSON.indvs)
  allnms <- unique(unlist(lapply(pull, names)))
  pull <- do.call(rbind,
                  c(lapply(pull,
                           function(x) data.frame(c(x, sapply(setdiff(allnms, names(x)),
                                                              function(y) NA)))),
                    make.row.names=FALSE))

  pull$inventory_status[pull$inventory_status==10] <- "active"
  pull$inventory_status[pull$inventory_status==12] <- "suspended"
  pull$updated_at <- gsub("Z","", gsub("T"," ",pull$updated_at))
  return(pull)
}
