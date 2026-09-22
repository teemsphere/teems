#' @noRd
#' @keywords internal
.shk_write_order <- function(free_idx, full_dimsizes) {
  n <- length(full_dimsizes)
  if (n <= 1L) {
    write_order <- seq_along(free_idx)
    return(write_order)
  }
  antidim <- numeric(n)
  antidim[1L] <- 1L
  for (k in seq_len(n - 1L) + 1L) {
    antidim[k] <- antidim[k - 1L] * full_dimsizes[k - 1L]
  }
  strides <- antidim[free_idx]
  o <- order(strides, decreasing = TRUE)
  return(o)
}
