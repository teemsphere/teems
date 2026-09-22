#' @importFrom tibble as_tibble
#' @keywords internal
#' @noRd
.probe_stats <- function(stats) {
  if (is.null(stats)) {
    return(NULL)
  }
  keep <- c(
    "version", "vecsize", "nvarele", "nexo", "nbacksolve", "nbselems",
    "matrix_method", "solution_method", "mpi_size", "bordered",
    "chain_source", "partition_source", "chain_set", "partition_set",
    "ntime", "nreg", "ndblock", "netcut", "nintraeq", "border_neq"
  )
  out <- stats[intersect(keep, names(stats))]
  nnz <- stats$realized$entries %|||% stats$structural$entries
  if (!is.null(nnz)) {
    out$nnz <- as.numeric(nnz)
  }
  pa <- stats$partition_auto
  if (!is.null(pa)) {
    cand <- if (is.data.frame(pa)) {
      pa
    } else {
      pa$candidates
    }
    if (!is.null(cand) && NROW(cand)) {
      out$partition_auto <- tibble::as_tibble(cand)
    }
    if (!is.data.frame(pa) && !is.null(pa$chosen)) {
      out$partition_chosen <- pa$chosen
    }
  }
  return(out)
}
