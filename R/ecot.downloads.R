#' Download individual information based on a date
#'
#' @description ecot.downloads makes a serie of request to Ecotopia API based on the choosen type of data, device id and date (optional) for a defined number (optional) of resulting data.
#'
#' @param token a character string giving your token for an open session. For obtaining it, see ecot.token.
#' @param device_id a character string of the device id from the table of devices information obtained with the API. For obtaining it, see ecot.indvs. The 3 different identifiers (uuid, device number, and device id) that appears on the Ecotopia web are not useful for this.
#' @param ndevicelimit the maximun number of data to download on each request to the API. The maximun available is 1000, which is the default value.
#' @param type a character string indicating the kind of data to download. It can be "GPS", "Env", "ODBA" or "Acc". The default value is "GPS".
#' @param maxrounds the maximun number of requests to the API. It means that the maximun number of rows donwloaded would be this value multiplied by the value selected on ndevicelimit.
#' @param datestart_updates a character string giving a date on the format "Y%-%m-%d %H:%M:$S" indicating date from which start the downloading. If this is not provided the downloads will start at first date according to "Deployment date" on Ecotopia webpage.
#' @param show_count a logical (TRUE or FALSE) indicating if an indicator of the current data that it is downloading should appears. The indicator is just a string with the last date of each 1e3 rows.
#'
#' @return a list of data frame. Each element of the list is the downloadED information for each individual.
#' @export
#'
#' @examples ecot.downloads(ecot.token("abc","passw"), "6267d0fae75c8ef26173d757", type = "Env")
#'
ecot.downloads <- function(token, device_id, ndevicelimit = 1e3, type = "GPS", maxrounds = NA, datestart_updates = NA, show_count = F){

  if(!is.na(datestart_updates))
    datestart_updates <- gsub(" ","T",paste0(datestart_updates,"Z"))

  Tloop <- ecot.API.request(token,device_id,ndevicelimit,datestart_updates,type)

  T_res <- Tloop
  listlength <- 1
  stop <- "NO"

  while(length(Tloop)==ndevicelimit & stop == "NO"){

    if(!is.na(maxrounds))
      if(listlength==maxrounds){
        stop <- "SI"
        next
      }

    if(show_count)
      cat(as.character(gsub("Z","",gsub("T"," ",Tloop[[length(Tloop)]]$timestamp))),"\n")
    to_start <- Tloop[[length(Tloop)]]$timestamp
    Tloop <- ecot.API.request(token, device_id, ndevicelimit, to_start, type)
    T_res <- c(T_res,Tloop)
    listlength <- listlength+1
  }

  if(length(Tloop)>0){ ## last downloaded date is always shown, not matter the alue of showcount
    maxdate <- Tloop[[length(Tloop)]]$timestamp
    cat(as.character(gsub("Z","",gsub("T"," ",maxdate))),"\n")

    return(T_res)
  } else {
    return(NULL)
  }


}
