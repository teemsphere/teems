#' @description The partition the solver would apply at `n_tasks`,
#'   replayed from the probe's candidate table with the solver's own
#'   rule (viable + at least `n_tasks` blocks; smallest border; near
#'   ties within 2% broken by block balance). `border_share` is the
#'   larger of border variables (netcut) and border equations
#'   (`border_neq`, known for the probe's chosen set only) over the
#'   system size. `NULL` when no candidate qualifies.
#' @keywords internal
#' @noRd
.auto_partition <- function(structure,
                            n_tasks) {
  cand <- structure$partition_auto
  if (is.null(cand) || !NROW(cand)) {
    return(NULL)
  }
  cand <- as.data.frame(cand)
  ok <- cand[cand$viable %in% TRUE & cand$nblocks >= n_tasks, , drop = FALSE]
  if (!NROW(ok)) {
    return(NULL)
  }
  best_cut <- min(ok$netcut)
  ok <- ok[50 * ok$netcut <= 51 * best_cut, , drop = FALSE]
  balance <- ok$block_min / pmax(ok$block_max, 1)
  pick <- ok[which.max(balance), , drop = FALSE]
  if (NROW(pick) > 1L) {
    pick <- pick[1L, , drop = FALSE]
  }
  vecsize <- structure$vecsize %|||% NA_real_
  border_neq <- if (identical(pick$set, structure$partition_set)) {
    structure$border_neq %|||% NA_real_
  } else {
    NA_real_
  }
  border <- max(pick$netcut, border_neq, na.rm = TRUE)
  partition <- list(
    set = pick$set,
    n_blocks = as.integer(pick$nblocks),
    netcut = as.integer(pick$netcut),
    border_neq = if (is.na(border_neq)) {
      NA_integer_
    } else {
      as.integer(border_neq)
    },
    block_min = as.integer(pick$block_min),
    block_max = as.integer(pick$block_max),
    border_share = if (is.na(vecsize) || vecsize <= 0) {
      NA_real_
    } else {
      border / vecsize
    }
  )
  return(partition)
}
