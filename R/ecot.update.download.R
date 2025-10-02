#' Download specific devices from specific dates
#'
#' @param user a character string giving to your user name account in Ecotopia. Only neccesary if token is not provided.
#' @param psw a character string giving the password of your account in Ecotopia. Only neccesary if token is not provided.
#' @param token a character string giving your token for an open session.  Only neccesary if user and psw is not provided. For obtaining it, see ecot.token.
#' @param type a character string indicating the kind of data to download. It can be "GPS", "Env", "ODBA" or "Acc"
#' @param maxrounds the maximun number of requests to the API. It means that the maximun number of data donwloaded by individual would be this value multiplied by 1000 (maximun available to be downloaded on one request).
#' @param devID a character vector giving the device id to update. The identifier to use could be uuid, device number according to Ecotopia webpage or the one from the table of devices information obtained with a call to the API. For obtaining the last one, see ecot.indvs.
#' @param max_dates a character vector giving the date of the last data already download of each device on the format "%Y-%m-%d %H:%M:%S". If this is not provided the downloads will start at first date according to "Deployment date" on Ecotopia webpage. This argument admits tenths of a second which is normally the case for acc data.
#' @param show_count a logical (TRUE or FALSE) indicating if an indicator of the current data that it is downloading should appears. The indicator is just a string with the last date of each 1e3 rows.
#'
#' @return a data frame containing all the available information that the Ecotopia API provide for each type of data. In the case of accelerometer data, the function returns a list of 2 elements, the first one containing the information for each acc measurement and the second containing al the acc samples corresponding to each measurement.
#' @export
#'
#' @examples
#' \dontrun{
#' T_Env <- ecot.whole.download("abc","passw", type = "Env")
#' }
#'
ecot.update.download <-   function(user, psw, token, type = "GPS", devID, max_dates, maxrounds = NA, show_count = F){

    if(missing(token) & (missing(user) | missing(psw)))
      stop("You need to provide either a token or your user and psw.")

    if(missing(token))
      token <- ecot.token(user,psw)

    if(missing(devID) | missing(max_dates))
      stop("You need to provide a vector with device id for each individual (devID) and a vector of the same length with the last date downloaded for each one (max_dates).")

    if(!missing(devID) & !missing(max_dates))
      if(length(devID)!=length(max_dates))
        stop("You need to provide a vector with device id for each individual (devID) and a vector of the SAME length with the last date downloaded for each one (max_dates).")

    Indv_id <- ecot.indvs(token = token)

    ## if devID provide uuid, this will extract desired devices API ids
    XX <- merge(data.frame(uuid = devID), Indv_id)
    ## if devID provide Ecotopia webpage device number, this will extract desired devices id from the API
    if(nrow(XX)==0)
      XX <- merge(data.frame(mark = devID), Indv_id)
    if(nrow(XX)!=0)
      devID_s <- XX$id else
        devID_s <- devID

    devID_act <- devID_s[devID_s %in% Indv_id$id]

    ## equivalences to cat
    devices_toshow <- merge(data.frame(id=devID_act),Indv_id)$mark

    Tres <- list()

    indv_loop <- 1
    error_count <- 1


    while(indv_loop <= length(devID_act)){

      if(error_count > nrow(Indv_id)*2) # the error that motivate the use of tryCatch normally appears once by each whole download.
        stop() ## such error will appear normally once, if there is any other error that keeps appearing, this line will crack the download instead of create an infinite loop.

      down_f <- function() ecot.downloads(token = token, device_id = devID_act[indv_loop],
                                          type = type, maxrounds = maxrounds, show_count = show_count,
                                          datestart_updates = max_dates[indv_loop])

      download.messages.loop_values(devices_toshow,indv_loop,error_count,Tres,down_f, token, type, maxrounds, show_count)

    } ## end while

    return(Tres)

  }







