#' @keywords internal
#' @noRd
.finalize_shocks <- function(shock,
                             ...) {
  UseMethod(".finalize_shocks")
}
#' @keywords internal
#' @noRd
#' @export
#' @method .finalize_shocks default
.finalize_shocks.default <- function(shock,
                                     shock_file,
                                     closure,
                                     sets,
                                     var_extract,
                                     ...) {

  if (is.null(shock_file)) {
    shock <- .shock_load(
      shocks = shock,
      closure = closure,
      sets = sets,
      var_extract = var_extract
    )
  } else {
    shock <- .usr_shock(shock_file = shock_file)
  }

  class(shock) <- c("shock", class(shock))
  return(shock)
}
#' @keywords internal
#' @noRd
#' @export
#' @method .finalize_shocks NULL
.finalize_shocks.NULL <- function(shock,
                                  ...) {
  shock <- structure(NA,
    file = "null_shock.shf",
    class = c("shock", class(NA))
  )
  return(shock)
}