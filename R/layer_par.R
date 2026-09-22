#' @importFrom purrr imap map
#' @importFrom stats setNames
#' @keywords internal
#' @noRd
.layer_par <- function(spec, i_data, fmt) {
  new_par <- purrr::imap(spec$par, \(p, header) {
    dims <- purrr::map(
      stats::setNames(p$dims, p$dims),
      \(d) .layer_elements(i_data, d)
    )
    value <- if (is.na(p$on)) {
      p$hi
    } else {
      ifelse(dims[[1]] %in% .layer_elements(i_data, p$on), p$hi, p$lo)
    }
    arr <- if (length(dims) == 0L) {
      array(value, dim = 1L)
    } else {
      array(value, dim = lengths(dims), dimnames = dims)
    }
    class(arr) <- c(header, "par", fmt, class(arr))
    arr
  })
  return(new_par)
}
