#' @keywords internal
#' @noRd
.implement_aux <- function(args_list,
                           call) {
  v <- .validate_aux_args(
    a = args_list,
    call = call
  )

  aux <- list()
  aux$dat <- .load_aux(
    aux = v$dat,
    data_type = "dat",
    call = call
  )

  aux$par <- .load_aux(
    aux = v$par,
    data_type = "par",
    call = call
  )

  aux$set <- .load_aux(
    aux = v$set,
    data_type = "set",
    call = call
  )

  aux <- unlist(aux, recursive = F, use.names = FALSE)
  attr(aux, "call") <- call
  return(aux)
}