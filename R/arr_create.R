#' @keywords internal
#' @noRd
.create_v7arr <- function(input,
                          value,
                          ...) {

  set_env <- list(...)
  sets <- unlist(coeff_conversion[coeff_conversion$v7.0header %in% input,
                                  "v7.0set"])

  sets <- with(set_env, mget(sets))
  arr <- array(
    data = value,
    dim = lengths(sets),
    dimnames = sets
  )

  class(arr) <- c(input, "par", "v7.0", class(arr))
  return(arr)
}
