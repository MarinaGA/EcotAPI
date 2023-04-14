#' Download the whole data base
#'
#' @param user a character string giving to your user name account in Ecotopia. Only neccesary if token is not provided.
#' @param psw a character string giving the password of your account in Ecotopia. Only neccesary if token is not provided.
#' @param token a character string giving your token for an open session.  Only neccesary if user and psw is not provided. For obtaining it, see ecot.token.
#' @param type a character string indicating the kind of data to download. It can be "GPS", "Env", "ODBA" or "Acc"
#' @param maxrounds the maximun number of requests to the API. It means that the maximun number of data donwloaded by individual would be this value multiplied by 1000 (maximun available to be downloaded on one request).
#' @param devID a character vector giving the device id to update. Only those active will be updated. the identifier to use is the one from the table of devices information obtained with a call to the API. For obtaining it, see ecot.indvs. The 3 different identifiers (uuid, device number, and device id) that appears on the Ecotopia webpage are not useful for this.
#' @param max_dates a character vector giving the date of the last data alrady download of each device on the format "Y%-%m-%d %H:%M:$S". If this is not provided the downloads will start at first date according to "Deployment date" on Ecotopia webpage. This argument admits tenths of a second which is normally the case for acc data.
#' @param show_count a logical (TRUE or FALSE) indicating if an indicator of the current data that it is downloading should appears. The indicator is just a string with the last date of each 1e3 rows.
#'
#' @return a data frame containing all the available information that the Ecotopia API provide for each type of data. Only information from active devices can be downloaded. In the case of accelerometer data, the function returns a list of 2 elements, the first one containing the information for each acc measurement and the second containing al the acc samples corresponding to each measurement.
#' @export
#'
#' @examples T_Env <- ecot.whole.download("abc","passw", type = "Env")
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



    Indv_id <- ecot.indvs(token)
    ## indvs suspended return the message 403 forbiden
    Indvs_susp <- subset(Indv_id,inventory_status == "suspended")
    ## indvs NOT suspended works
    Indvs_act <- subset(Indv_id,inventory_status == "active")

    ## only active devices can be updated
    devID_act <- c(Indvs_act$id, devID)
    devID_act <- devID_act[duplicated(devID_act)]

    Tres <- list()

    for(i in 1:length(devID_act)){
      cat("\nIndv",i,"\n\n")
      Tloop <- ecot.downloads(token = token, device_id = devID_act[i], type = type, maxrounds = maxrounds, datestart_updates = max_dates[i], show_count = show_count)
      if(!is.null(Tloop)){
        tobind <- ecot.JSON_to_df(Tloop) ## this function is not inside the previous one because testing is easier this way
        if(type != "Acc")
          Tres[[i]] <- tobind else
            Tres <- list(sample_info = rbind(Tres$sample_info,tobind$sample_info),acc = c(Tres$acc,tobind$acc))
      }
    }

    return(Tres)

  }
