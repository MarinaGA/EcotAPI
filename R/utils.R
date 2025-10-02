# Declaración de variables globales para evitar notas de R CMD check
if(getRversion() >= "2.15.1") {
  utils::globalVariables(
    c(
      "inventory_status"  # variable que viene de la descarga
    )
  )
}
