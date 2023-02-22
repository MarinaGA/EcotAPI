#' Transforn lists from JSON files to data.frames
#'
#' @description Transforn lists resulting from a JSON files to a data frame. It is only for JSON files obtained from a call to the API intended to obtain GPS data from network, ODBA data, Environmental data or accelerometer raw data.
#'
#' @param y a list from a JSON file of GPS/Env/ODBA/Acc data after applying content(x, as = "parsed")
#'
#' @return a list of data frames each one containing information of one device.
#' @export
#'
#' @examples ## not to run, this is used inside ecot.updated.download
#' @examples Tloop <- ecot.downloads(token = token, indv_id = devID_act[i], type = type, maxrounds = maxrounds, datestart_updates = max_dates[i])
#' @examples if(!is.null(Tloop)){
#' @examples tobind <- ecot.JSON_to_df(Tloop)
#'
ecot.JSON_to_df <- function(y){

  split_f <- function(z) {
    listlength <- sapply(z, length)
    num_diflen <- which(listlength < max(listlength))

    if(length(num_diflen) > 0){
      z_later <- z[num_diflen]
      z <- z[-num_diflen]
      return(list(done = z, to.do = z_later))
    }
    return(list(done = z, to.do = NULL))
  }

  core_f <- function(x) {

    x1 <- x[[1]] ## debe estar aqui para que siempre coja el maximo para los nombres

    listloop <- list()
    xyz_logic <- F

    for(z in 1:length(x1)){

      if (inherits(x1[[z]], "list")) {

        if (names(x1[z]) == "sample_type") {

          valores <- lapply(x,`[[`,z)
          valores <- lapply(valores, function(x) do.call(c,x))

          valores1 <- sapply(valores,`[[`,1)
          dt <- data.frame(sample_type1 = valores1)

          num <- which(sapply(valores, length)==2)
          valores2 <- sapply(valores[num],`[[`,2)
          dt$sample_type2 <- NA
          if(length(valores2)>0)
            dt$sample_type2[num] <- valores2

          num <- which(sapply(valores, length)==3)
          valores3 <- sapply(valores[num],`[[`,3)
          dt$sample_type3 <- NA
          if(length(valores3)>0)
            dt$sample_type3[num] <- valores3

          listloop[[z]] <- dt

        }

        # if (names(x1[z]) == "loc") { ## NO ME INTERESA, IGUAL QUE LAS OTRAS COORDS...
        #   dt <- as.data.frame(t(do.call(c, x1$loc$coordinates)))
        #   colnames(dt) <- c("longitude","latitude")}

        if (names(x1[z]) == "satellite") {

          valores <- lapply(x,`[[`,z)
          valores <- lapply(valores, function(x) do.call(c,x))
          valores <- lapply(valores, function(x) do.call(c,x))
          nm <- names(valores[[1]])
          dt <- as.data.frame(do.call(rbind,lapply(valores, as.numeric)))
          colnames(dt) <- paste0("satellite.", nm)
          listloop[[z]] <- dt
        }

        if (names(x1[z]) == "xyz") {
          xyz_logic <- T
          valores <- lapply(x,`[[`,z)
          listxyz <- lapply(valores,
                            function(k){
                              return(data.frame(x = sapply(k,`[[`,2), y = sapply(k,`[[`,3), Z = sapply(k,`[[`,4)))})
        }
      } else {

        valores <- do.call(c,lapply(x,`[[`,z))
        nm <- names(x1)[[z]]
        eval(parse(text = paste0("dt <- data.frame(",nm," = valores)")))
        listloop[[z]] <- dt

      }
    }

    listloop <- listloop[lengths(listloop) != 0]
    xp <- do.call(cbind,listloop)

    if(xyz_logic)
      xp <- list(sample_info = xp, acc = listxyz)


    res <- xp

    if(xyz_logic){
      res$sample_info$updated_at <- as.character(gsub("Z","",gsub("T"," ",res$sample_info$updated_at)))
      res$sample_info$timestamp  <- as.character(gsub("Z","",gsub("T"," ",res$sample_info$timestamp)))
      res$sample_info$timestamp_end  <- as.character(gsub("Z","",gsub("T"," ",res$sample_info$timestamp_end)))
    } else {
      res$updated_at <- as.character(gsub("Z","",gsub("T"," ",res$updated_at)))
      res$timestamp  <- as.character(gsub("Z","",gsub("T"," ",res$timestamp)))
    }

    assign("xyz_logic",xyz_logic, envir = parent.frame())
    return(res)

  }

  y <- split_f(y)
  tosave <- core_f(y$done)

  while(length(y$to.do)>0){
    y <- split_f(y$to.do)
    tosaveloop <- core_f(y$done)
    if(!xyz_logic)
      tosave <- rbind.fill(tosave,tosaveloop) else
        tosave <- list(sample_info = rbind(tosave$sample_info,tosaveloop$sample_info),acc = c(tosave$acc,tosaveloop$acc))
  }

  return(tosave)

}
