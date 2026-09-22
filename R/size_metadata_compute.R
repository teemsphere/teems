#' @importFrom purrr map_dbl
#' @keywords internal
#' @noRd
.compute_size_metadata <- function(var_extract,
                                   sets,
                                   closure) {
  n_var_ele <- .count_var_elements(
    var_extract = var_extract,
    sets = sets
  )

  n_exo_ele <- sum(purrr::map_dbl(
    closure,
    \(entry) {
      ele <- attr(entry, "ele")
      if (ele %=% NA) {
        return(1)
      }
      nrow(ele)
    }
  ))

  metadata <- list(
    system_size = n_var_ele - n_exo_ele,
    n_var_ele = n_var_ele,
    n_exo_ele = n_exo_ele,
    n_reg = length(sets$ele$REG)
  )
  return(metadata)
}
