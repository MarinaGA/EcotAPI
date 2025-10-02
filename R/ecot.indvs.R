#' Download devices information
#'
#' @param user a character string giving to your user name account in Ecotopia.
#' @param psw a character string giving the password of your account in Ecotopia.
#' @param token a character string giving your token for an open session. For obtaining it, see ecot.token.
#' @param ndevicelimit the maximun number of data to download on each request to the API. The maximun available is 1000, which is the default value.
#'
#' @return a data.frame with devices information available. The column "id" contains the useful device id for the API request.
#' @export
#'
#' @importFrom utils install.packages installed.packages
#' @importFrom httr GET add_headers content
#' @importFrom data.table rbindlist
#'
#' @examples
#' \dontrun{
#' devices_info <- ecot.indvs(ecot.token("abc","passw"))
#' }
#'
ecot.indvs <- function(user, psw, token, ndevicelimit = 1e3){

  list.of.packages <- c("httr", "data.table") ## needed packages
  new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
  if (length(new.packages)) install.packages(new.packages)
  invisible(lapply(list.of.packages, library, character.only = TRUE))

  if(missing(token) & (missing(user) | missing(psw)))
    stop("You need to provide either a token or your user and psw.")

  if(missing(token))
    token <- ecot.token(user,psw)

  headers <-
    add_headers("x-druid-authentication" = token, "x-result-limit" = ndevicelimit)
  url <- "https://www.ecotopiago.com/api/v3/device/page/"
  input <- GET(url, config = headers)

  pull <- rbindlist(lapply(lapply(content(input), unlist), as.list), fill = TRUE)

  pull$inventory_status[pull$inventory_status==10] <- "active"
  pull$inventory_status[pull$inventory_status==12] <- "suspended"
  pull$updated_at <- gsub("Z","", gsub("T"," ",pull$updated_at))
  return(pull)
}
