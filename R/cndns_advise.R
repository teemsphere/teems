.cndns_lu_size <- 1e6

#' @keywords internal
#' @noRd
.advise_cndns <- function(metadata,
                          matrix_method,
                          enable_time,
                          call) {
  condense <- metadata$condense
  if (is.null(condense) || (condense$n_backsolve %|||% 0L) < 1L) {
    return(invisible(NULL))
  }
  n_backsolve <- condense$n_backsolve
  share <- .cndns_share(condense)

  if (enable_time) {
    .cli_action(solve_info$condense_intertemporal,
      action = rep("inform", 3),
      call = call
    )
  } else if (matrix_method %in% c("SBBD", "DBBD", "NDBBD")) {
    .cli_action(solve_info$condense_bordered,
      action = rep("inform", 3),
      call = call
    )
  }
  return(invisible(NULL))
}
