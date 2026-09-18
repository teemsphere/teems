#' @description Projected MA48 workspace for a single sequential LU
#'   factorization, and whether it clears the 32-bit ceiling. Returns
#'   `NULL` when the probe supplied no nonzero count (the exclusion
#'   cannot be applied without one). `condensed` selects the fill
#'   anchor: condensation cuts nnz but raises fill by about as much.
#' @keywords internal
#' @noRd
.auto_lu_ceiling <- function(nnz,
                             condensed = FALSE,
                             th = .auto_thresholds()) {
  if (is.null(nnz) || is.na(nnz) || nnz <= 0) {
    return(NULL)
  }
  fill <- if (isTRUE(condensed)) {
    th$lu_fill_condensed
  } else {
    th$lu_fill
  }
  projected <- nnz * fill
  share <- projected / th$lu_la_ceiling
  ceiling <- list(
    nnz = nnz,
    fill = fill,
    projected = projected,
    ceiling = th$lu_la_ceiling,
    share = share,
    exceeded = projected >= th$lu_la_ceiling,
    near = share >= th$lu_ceiling_warn_share
  )
  return(ceiling)
}
