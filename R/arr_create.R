#' @keywords internal
#' @noRd
.create_GTAPv7arr <- function(input,
                              value,
                              ...) {

  set_env <- list(...)
  sets <- unlist(coeff_conversion[coeff_conversion$GTAPv7header %in% input,
                                  "GTAPv7set"])

  sets <- with(set_env, mget(sets))
  arr <- array(
    data = value,
    dim = lengths(sets),
    dimnames = sets
  )

  class(arr) <- c(input, "par", "GTAPv7", class(arr))
  return(arr)
}
