#' Transforn lists from JSON files to data.frames
#'
#' @description Transforn lists resulting from a JSON files to a data frame. It is only for JSON files obtained from a call to the API intended to obtain devices information.
#'
#' @param x a list from a JSON file of devices information after applying content(x, as = "parsed")
#'
#' @return a list of data frames each one containing information of one device.
#' @export
#'
#' @examples pull <- lapply(input, ecot.unlist.JSON.indvs) # used inside ecot.indv
#'
ecot.unlist.JSON.indvs <- function(x) {

  lchar <- list()
  lint <- list()
  llist <- list()

  for (z in seq_along(names(x))) {
    if (inherits(x[[z]], "character")) {
      nm <- names(x)[[z]]
      valor <- as.character(x[[z]])
      dt <- data.frame(variable = nm, valor = valor)
      lchar[z] <- list(dt)
    }

    if (inherits(x[[z]], "integer")) {
      nm <- names(x)[[z]]
      valor <- as.character(x[[z]])
      dt <- data.frame(variable = nm, valor = valor)
      lint[z] <- list(dt)
    }

    if (inherits(x[[z]], "list")) {
      if (names(x[z]) == "biological_ref") {
        biological_ref <-
          as.data.frame(t(do.call(
            rbind.data.frame, x$biological_ref
          )))
        colnames(biological_ref) <- names(x[[z]])
        row.names(biological_ref) <- seq(1:nrow(biological_ref))
        colnames(biological_ref)[2] <- paste0("biological_ref.", names(biological_ref)[2])
        llist [z] <- list(biological_ref)
      }
      if (names(x[z]) == "status_device") {
        status_device <-
          as.data.frame(t(do.call(
            rbind.data.frame, x$status_device
          )))
        colnames(status_device) <- names(x[[z]])
        colnames(status_device) <-
          paste0("status_device.", names(status_device))
        row.names(status_device) <- seq(1:nrow(status_device))
        llist [z] <- list(status_device)
      }
      if (names(x[z]) == "status_env") {
        status_env <-
          as.data.frame(t(do.call(
            rbind.data.frame, x$status_env
          )))
        colnames(status_env) <- names(x[[z]])
        colnames(status_env) <-
          paste0("status_env.", names(status_env))
        row.names(status_env) <- seq(1:nrow(status_env))
        llist [z] <- list(status_env)
      }
      if (names(x[z]) == "status_gps") {
        status_gps <-
          as.data.frame(t(do.call(
            rbind.data.frame, x$status_gps
          )))
        colnames(status_gps) <- names(x[[z]])
        colnames(status_gps) <-
          paste0("status_gps.", names(status_gps))
        row.names(status_gps) <- seq(1:nrow(status_gps))
        llist [z] <- list(status_gps)
      }
      if (names(x[z]) == "product") {
        product_id <- x[[z]]$product_id
        product_series <- x[[z]]$product_series
        product_series_child <- x[[z]]$product_series_child
        special_device <- x[[z]]$special_device[[1]]
        battery_voltage_level.boost1 <-
          x[[z]]$battery_voltage_level[[1]]
        battery_voltage_level.boost1 <-
          as.data.frame(t(
            do.call(rbind.data.frame, battery_voltage_level.boost1)
          ))
        colnames(battery_voltage_level.boost1) <-
          names(x[[z]]$battery_voltage_level[[1]])
        colnames(battery_voltage_level.boost1) <-
          paste0("battery_voltage_level.boost1.",
                 names(battery_voltage_level.boost1))
        row.names(battery_voltage_level.boost1) <-
          seq(1:nrow(battery_voltage_level.boost1))

        battery_voltage_level.boost2 <-
          x[[z]]$battery_voltage_level[[2]]
        battery_voltage_level.boost2 <-
          as.data.frame(t(
            do.call(rbind.data.frame, battery_voltage_level.boost2)
          ))
        colnames(battery_voltage_level.boost2) <-
          names(x[[z]]$battery_voltage_level[[2]])
        colnames(battery_voltage_level.boost2) <-
          paste0("battery_voltage_level.boost2.",
                 names(battery_voltage_level.boost2))
        row.names(battery_voltage_level.boost2) <-
          seq(1:nrow(battery_voltage_level.boost2))

        battery_voltage_level.boost3 <-
          x[[z]]$battery_voltage_level[[3]]
        battery_voltage_level.boost3 <-
          as.data.frame(t(
            do.call(rbind.data.frame, battery_voltage_level.boost3)
          ))
        colnames(battery_voltage_level.boost3) <-
          names(x[[z]]$battery_voltage_level[[3]])
        colnames(battery_voltage_level.boost3) <-
          paste0("battery_voltage_level.boost3.",
                 names(battery_voltage_level.boost3))
        row.names(battery_voltage_level.boost3) <-
          seq(1:nrow(battery_voltage_level.boost3))

        product <-
          data.frame(
            product_id = product_id,
            product_series = product_series,
            product_series_child = product_series_child,
            special_device = special_device
          )
        colnames(product) <- paste0("product.", names(product))
        product <-
          cbind(
            product,
            battery_voltage_level.boost1,
            battery_voltage_level.boost2,
            battery_voltage_level.boost3
          )
        llist [z] <- list(product)
      }
    }
  }

  table.chr <- as.data.frame(t(do.call(rbind.data.frame, lchar)))
  colnames(table.chr) <-
    table.chr[row.names(table.chr) == "variable",]
  table.chr <- table.chr[row.names(table.chr) != "variable",]

  if(class(table.chr) != "data.frame"){
    table.chr <- as.data.frame(t(do.call(rbind.data.frame, lchar)))
    colnames(table.chr) <-
      table.chr[row.names(table.chr) == "variable",]
    nnn <- table.chr[1,1]
    table.chr <- data.frame(nnn= table.chr[2,1])
    colnames(table.chr)=nnn
  }
  row.names(table.chr) <- seq(1:nrow(table.chr), 1)

  table.int <- as.data.frame(t(do.call(rbind.data.frame, lint)))
  colnames(table.int) <-
    table.int[row.names(table.int) == "variable",]
  table.int <- table.int[row.names(table.int) != "variable",]

  if(class(table.int) != "data.frame"){
    table.int <- as.data.frame(t(do.call(rbind.data.frame, lint)))
    colnames(table.int) <-
      table.int[row.names(table.int) == "variable",]
    nnn <- table.int[1,1]
    table.int <- data.frame(nnn= table.int[2,1])
    colnames(table.int)=nnn
  }
  row.names(table.int) <- seq(1:nrow(table.int), 1)

  llist <- llist[lengths(llist) != 0]
  table.list <- as.data.frame(do.call(cbind.data.frame, llist))

  xp <- cbind(table.chr, table.int, table.list)
  "%ni%" <- Negate("%in%")
  if ("attached_at" %ni% names(xp)) {
    xp$attached_at <- NA
    ncols <- ncol(xp)
    xp <- xp[, c(1:15, ncols, 16:ncols)]
  }
  xp <- xp[, !grepl(".1", names(xp), fixed = T)]
  return(xp)
}
