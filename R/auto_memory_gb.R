#' @keywords internal
#' @noRd
.auto_memory_gb <- function(method,
                            n_tasks,
                            plain_size,
                            condensed = FALSE,
                            th = .auto_thresholds()) {
  if (is.null(plain_size) || is.na(plain_size) || plain_size <= 0) {
    return(NA_real_)
  }
  n_tasks <- max(1L, as.integer(n_tasks))
  kb <- switch(method,
    LU = th$mem_lu * (if (isTRUE(condensed)) {
      th$mem_lu_condensed
    } else {
      1
    }),
    DBBD = (th$mem_dbbd_base + th$mem_dbbd_rank * n_tasks) *
      (if (isTRUE(condensed)) {
        th$mem_dbbd_condensed
      } else {
        1
      }),
    SBBD = th$mem_sbbd_base + th$mem_sbbd_rank * n_tasks,
    NDBBD = th$mem_ndbbd_rank * n_tasks + th$mem_ndbbd_local,
    NA_real_
  )
  return(kb * plain_size / 1e6)
}
