#' @keywords internal
#' @noRd
.probe_cores <- function(fine) {
  if (is.null(fine)) {
    return(NULL)
  }
  sizes <- if (is.null(fine$core_sizes) || !NROW(fine$core_sizes)) {
    tibble::tibble(size = integer(), count = integer())
  } else {
    tibble::tibble(
      size = as.integer(fine$core_sizes$size),
      count = as.integer(fine$core_sizes$count)
    )
  }
  top <- fine$top_cores
  top_tbl <- if (is.null(top) || !NROW(top)) {
    tibble::tibble(core = integer(), size = integer(), eqs = list(), vars = list())
  } else {
    tibble::tibble(
      core = seq_len(NROW(top)),
      size = as.integer(top$size),
      eqs = lapply(top$eqs, .probe_agg_tbl),
      vars = lapply(top$vars, .probe_agg_tbl)
    )
  }
  cores <- list(
    sq_comps = fine$sq_comps %|||% NA_integer_,
    cores_gt1 = fine$cores_gt1 %|||% NA_integer_,
    largest = fine$largest_core %|||% NA_integer_,
    sizes = sizes,
    top = top_tbl
  )
  return(cores)
}
