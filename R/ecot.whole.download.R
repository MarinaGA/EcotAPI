#' Download the whole data base
#'
#' @param user a character string giving to your user name account in Ecotopia. Only neccesary if token is not provided.
#' @param psw a character string giving the password of your account in Ecotopia. Only neccesary if token is not provided.
#' @param token a character string giving your token for an open session.  Only neccesary if user and psw is not provided. For obtaining it, see ecot.token.
#' @param type a character string indicating the kind of data to download. It can be "GPS", "Env", "ODBA" or "Acc"
#' @param maxrounds the maximun number of requests to the API. It means that the maximun number of data donwloaded by individual would be this value multiplied by 1000 (maximun available to be downloaded on one request).
#' @param show_count a logical (TRUE or FALSE) indicating if an indicator of the current data that it is downloading should appears. The indicator is just a string with the last date of each 1e3 rows.
#' @param max_devs the maximun number of active devices to download. Optional.
#'
#' @return a data frame containing all the available information that the Ecotopia API provide for each type of data. Only information from active devices can be downloaded. In the case of accelerometer data, the function returns a list of 2 elements, the first one containing the information for each acc measurement and the second containing al the acc samples corresponding to each measurement.
#' @export
#'
#' @examples T_Env <- ecot.whole.download("abc","passw", type = "Env")
#'
ecot.whole.download <-  function(user, psw, token, type = "GPS", maxrounds = NA, show_count = F, max_devs){

    if(missing(token) & (missing(user) | missing(psw)))
      stop("You need to provide either a token or your user and psw.")

    if(missing(token))
      token <- ecot.token(user,psw)

    Indv_id <- ecot.indvs(token = token)
    ## indvs suspended return the message 403 forbiden
    Indvs_susp <- subset(Indv_id,inventory_status == "suspended")
    ## indvs NOT suspended works
    Indvs_act <- subset(Indv_id,inventory_status == "active")

    ## equivalences to cat
    devices_toshow <- Indvs_act$mark

    Tres <- list()

    indv_loop <- 1
    error_count <- 1

    if(missing(max_devs))
      max_devs <- nrow(Indvs_act)

    while(indv_loop <= max_devs){

      if(error_count > nrow(Indvs_act)*2) # the error that motivate the use of tryCatch normally appears once by each whole download.
        stop()

      down_f <- function() ecot.downloads(token = token, device_id = Indvs_act$id[indv_loop],
                                          type = type, maxrounds = maxrounds, show_count = show_count)

      download.messages.loop_values(devices_toshow,indv_loop,error_count,Tres,down_f, token, type, maxrounds, show_count)

    } ## end while

    return(Tres)

  }






