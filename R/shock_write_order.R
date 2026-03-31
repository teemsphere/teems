#' @noRd
#' @keywords internal
.shk_write_order <- function(free_idx, full_dimsizes) {
  n <- length(full_dimsizes)
  if (n <= 1L) {
    return(seq_along(free_idx))
  }
  antidim <- numeric(n)
  antidim[1L] <- full_dimsizes[2L]
  antidim[2L] <- 1L
  if (n >= 3L) antidim[3L] <- antidim[1L] * full_dimsizes[1L]
  for (k in seq_len(max(0L, n - 3L)) + 3L) {
    antidim[k] <- antidim[k - 1L] * full_dimsizes[k - 1L]
  }
  strides <- antidim[free_idx]
  o <- order(strides, decreasing = TRUE)
  return(o)
}