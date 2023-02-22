#' ecot.token
#'
#' @param user a string corresponding to your user name account in Ecotopia
#' @param psw a string corresponding to the password of your account in Ecotopia
#'
#' @return It returns a string corresponding to your token for a limited session of API communication
#' @export
#' @description ESTO ES DESCRIP
#'
#' @examples ecot.token("abc","passwd") # where your user names is "abc" and your password is "passwd"

ecot.token <- function(user,psw){

  list.of.packages <- c("httr", "jsonlite", "rjson","digest") ## needed packages
  new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
  if (length(new.packages)) install.packages(new.packages)
  invisible(lapply(list.of.packages, library, character.only = TRUE))

  ## codify the real password
  pswsha256 <- digest::digest(paste0(user," + druid + ",psw," + heifeng"), algo=c('sha256'), serialize = F)
  ## send the request to the API
  token <- httr::POST("https://ecotopiago.com/api/v2/login", body = paste0('{"username":"',user,'","password":"',pswsha256,'"}'))
  ## extract the token
  token_s <- token$headers$`x-druid-authentication`
  return(token_s)
}
