#' @importFrom rlang arg_match
#' 
#' @keywords internal
#' @noRd
.implement_aux <- function(args_list,
                           call) {

  v <- .validate_aux_args(
    a = args_list,
    call = call
  )

  aux <- .read_aux(
    input = v$input,
    type = v$type,
    header = v$header,
    call = call
  )

  if (!is.list(aux)) {
    attr(aux, "call") <- call
  } else {
    aux <- lapply(aux, \(a) {
      attr(a, "call") <- call
      return(a)
    })
  }

  return(aux)
}