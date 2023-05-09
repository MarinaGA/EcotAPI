#' Download all available data for the choosen devices
#'
#' @param user a character string giving to your user name account in Ecotopia. Only neccesary if token is not provided.
#' @param psw a character string giving the password of your account in Ecotopia. Only neccesary if token is not provided.
#' @param token a character string giving your token for an open session.  Only neccesary if user and psw is not provided. For obtaining it, see ecot.token.
#' @param type a character string indicating the kind of data to download. It can be "GPS", "Env", "ODBA" or "Acc"
#' @param devices a vector containing the devices number showed on Ecotopia webpage or the UUID to be downloaded. Devices should have the "Active" status.
#' @param maxrounds the maximun number of requests to the API. It means that the maximun number of data donwloaded by individual would be this value multiplied by 1000 (maximun available to be downloaded on one request).
#' @param show_count a logical (TRUE or FALSE) indicating if an indicator of the current data that it is downloading should appears. The indicator is just a string with the last date of each 1e3 rows.
#' @param max_devs the maximun number of active devices to download. Optional.
#'
#' @return a data frame containing all the available information that the Ecotopia API provide for each type of data. Only information from active devices can be downloaded. In the case of accelerometer data, the function returns a list of 2 elements, the first one containing the information for each acc measurement and the second containing al the acc samples corresponding to each measurement.
#' @export
#'
#' @examples T_Env <- ecot.whole.devices("abc","passw", type = "Env", devices = c(11125,11126,11135))
#'
ecot.whole.devices <-  function(user, psw, token, type = "GPS", devices, maxrounds = NA, show_count = F, max_devs){

    if(missing(token) & (missing(user) | missing(psw)))
      stop("You need to provide either a token or your user and psw.")

    if(missing(devices))
      stop("You need to provide a vector with devices id extracted from the API.")

    if(missing(token))
      token <- ecot.token(user,psw)

    Indv_id <- ecot.indvs(token)
    ## indvs suspended return the message 403 forbiden
    Indvs_susp <- subset(Indv_id,inventory_status == "suspended")
    ## indvs NOT suspended works
    Indvs_act <- subset(Indv_id,inventory_status == "active")

    ## if devices provide uuid, this will extract desired devices API ids
    XX <- merge(data.frame(uuid = devices), Indvs_act)
    ## if devices provide Ecotopia webpage device number, this will extract desired devices id from the API
    if(nrow(XX)==0)
      XX <- merge(data.frame(mark = devices), Indvs_act)

    Indvs_act <- XX ## this df should contain only desired devices API ids

    Tres <- list()

    indv_loop <- 1
    error_count <- 1

    if(missing(max_devs))
      max_devs <- nrow(Indvs_act)

    while(indv_loop <= max_devs){

      if(error_count > nrow(Indvs_act)*2){ # the error that motivate the use of tryCatch normally appears once by each whole download.
        print(paste("The indv",indv_loop, "produced an error and it was not donwloaded"))
        indv_loop <- indv_loop + 1
      }

      tryCatch({

        error_count <- error_count + 1

        cat("\nIndv",indv_loop,"\n\n") ## this shows the indv that is being downloaded
        Tloop <- ecot.downloads(token = token,device_id = Indvs_act$id[indv_loop], type = type, maxrounds = maxrounds, show_count = show_count)
        tobind <- ecot.JSON_to_df(Tloop) ## this function is not inside the previous one because testing is easier this way

        if(type != "Acc")
          Tres[[indv_loop]] <- tobind else
            Tres[[indv_loop]] <- list(sample_info = rbind(Tres$sample_info,tobind$sample_info),acc = c(Tres$acc,tobind$acc))

        indv_loop <- indv_loop+1

      },error=function(e) {message("The error '<simpleError: object of type 'externalptr' is not subsettable>', it is solved despite it appears sometimes");print(e)
      },warning=function(w) {message('A Warning Occurred');print(w)
      })
    }

    return(Tres)

  }
