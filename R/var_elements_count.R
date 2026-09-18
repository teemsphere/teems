#' @importFrom purrr map_dbl
#'
#' @keywords internal
#' @noRd
.count_var_elements <- function(var_extract,
                                sets) {
  n_elements <- sum(purrr::map_dbl(
    var_extract$ls_upper_idx,
    \(var_sets) {
      if (var_sets %=% NA) {
        return(1)
      }
      prod(lengths(with(sets$ele, mget(var_sets, ifnotfound = ""))))
    }
  ))
  return(n_elements)
}
