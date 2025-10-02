#' Inside function
#'
#' @description This function is only to avoid code repetition inside ecot.whole.download, ecot.update.download and ecot.whole.devices
#'
#' @param devices_toshow parameter already created inside the above mentioned functions
#' @param indv_loop parameter already created inside the above mentioned functions
#' @param error_count parameter already created inside the above mentioned functions
#' @param Tres parameter already created inside the above mentioned functions
#' @param down_f parameter already created inside the above mentioned functions
#' @param token parameter already created inside the above mentioned functions
#' @param type parameter already created inside the above mentioned functions
#' @param maxrounds parameter already created inside the above mentioned functions
#' @param show_count parameter already created inside the above mentioned functions
#'
#' @return updated values for indv_loop, error_count, and Tres  for each round of the loop of download.
#'
#' @export
#'
#' @importFrom data.table rbindlist
#'
#' @examples
#' \dontrun{
#' download.messages.loop_values(devices_toshow,indv_loop,error_count,Tres)
#' }
#' @keywords internal
#'
download.messages.loop_values <- function(devices_toshow,indv_loop,error_count,Tres, down_f, token, type, maxrounds, show_count){
tryCatch({

  error_count <- error_count + 1

  cat("\nIndv",indv_loop,"-",type,"-",devices_toshow[indv_loop],"\n\n") ## this shows the device that is being downloaded

  Tloop <- down_f()

  if(!is.null(Tloop)){

    if(type != "Acc"){

      tobind <- rbindlist(lapply(lapply(Tloop, unlist),as.list), fill = TRUE)
      Tres[[indv_loop]] <- tobind

    } else {

      numxyz <- grep("xyz",names(Tloop[[1]]))

      sample_info <- rbindlist(lapply(
        lapply(Tloop,function(x)unlist(x[1:(numxyz-1)]))
        ,as.list))

      xyzinfo <- lapply(
        lapply(Tloop,function(x)x[[numxyz]])
        ,rbindlist)

      # which(!grepl("uuid",colnames(x[[1]]))) ## I dont know how to introduce this in the apply.
      # xyzinfo <- lapply(xyzinfo[1:2],function(x)x <- x[,which(!grepl("uuid",colnames(x[[1]])))])
      xyzinfo <- lapply(xyzinfo,function(x)x <- x[,c(2:4)])

      Tres <- list(sample_info = rbind(Tres$sample_info,sample_info),acc = c(Tres$acc,xyzinfo))
    }

  } else {

    message("This device has not downloaded any data")}

  indv_loop <- indv_loop+1

},error=function(e) {message("The error '<simpleError: object of type 'externalptr' is not subsettable>', it is solved despite it appears sometimes");print(e)
},warning=function(w) {message('A Warning Occurred');print(w)
}) ## end tryCatch

  assign("indv_loop",indv_loop,envir = parent.frame())
  assign("error_count",error_count,envir = parent.frame())
  assign("Tres",Tres,envir = parent.frame())
}
