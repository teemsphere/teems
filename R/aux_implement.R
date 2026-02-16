#' @keywords internal
#' @noRd
.implement_aux <- function(args_list,
                           call) {
  
  if (all(is.null(args_list$dat),
          is.null(args_list$par),
          is.null(args_list$set))) {
    .cli_action(aux_err$no_inputs,
                action = "abort",
                call = call)
  }
  
  if (!is.null(args_list$set)) {
    .cli_action(aux_err$beta_set,
                action = "abort",
                call = call)
  }
  
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

  # aux$set <- .load_aux(
  #   aux = v$set,
  #   data_type = "set",
  #   call = call
  # )

  aux <- unlist(aux, recursive = F, use.names = FALSE)
  attr(aux, "call") <- call
  return(aux)
}