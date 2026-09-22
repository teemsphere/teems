#' @keywords internal
#' @noRd
.probe_cndns <- function(stats,
                         cmf_path) {
  if (is.null(stats)) {
    return(NULL)
  }
  metadata <- if (is.null(cmf_path)) {
    NULL
  } else {
    .deploy_metadata(cmf_path)
  }
  nominated <- metadata$condense

  vecsize <- stats$vecsize %|||% NA_integer_
  n_backsolve_ele <- stats$nbselems %|||% 0
  n_blocks <- stats$ndblock %|||% 0
  border <- stats$netcut
  uncondensed <- vecsize + n_backsolve_ele

  condensed <- n_backsolve_ele > 0
  partitioned <- isTRUE(stats$bordered) && n_blocks > 1

  verdict <- "none"
  if (condensed && partitioned) {
    verdict <- "hurts"
  } else if (condensed && !partitioned) {
    verdict <- "helps"
  } else if (!condensed && !partitioned && !is.na(vecsize) &&
    vecsize >= .cndns_lu_size) {
    verdict <- "candidate"
  }

  condense <- list(
    condensed = condensed,
    n_backsolve = stats$nbacksolve %|||% (nominated$n_backsolve %|||% 0L),
    n_backsolve_ele = n_backsolve_ele,
    elimination_share = if (isTRUE(uncondensed > 0)) {
      n_backsolve_ele / uncondensed
    } else {
      0
    },
    partitioned = partitioned,
    n_blocks = n_blocks,
    partition_set = stats$partition_set,
    chain_set = stats$chain_set,
    border = border,
    border_share = if (is.null(border) || is.na(vecsize) || vecsize <= 0) {
      NA_real_
    } else {
      border / vecsize
    },
    verdict = verdict
  )
  return(condense)
}
