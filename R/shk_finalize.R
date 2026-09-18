#' @keywords internal
#' @noRd
.finalize_shks <- function(shock,
                           ...) {
  return(UseMethod(".finalize_shks"))
}

#' @keywords internal
#' @noRd
#' @export
#' @method .finalize_shks default
.finalize_shks.default <- function(shock,
                                     closure,
                                     sets,
                                     var_extract,
                                     ...) {
  shock <- .shk_load(
    shocks = shock,
    closure = closure,
    sets = sets,
    var_extract = var_extract
  )

  class(shock) <- c("shock", class(shock))
  return(shock)
}

#' @keywords internal
#' @noRd
#' @export
#' @method .finalize_shks NULL
.finalize_shks.NULL <- function(shock,
                                  ...) {
  shock <- structure(NA,
    file = "null_shock.shf",
    class = c("shock", class(NA))
  )

  return(shock)
}

#' @keywords internal
#' @noRd
#' @export
#' @method .finalize_shks character
.finalize_shks.character <- function(shock,
                                       ...) {
  shock <- .usr_shk(shock_file = shock)
  return(shock)
}
