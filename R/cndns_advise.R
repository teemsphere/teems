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

  msg <- if (enable_time) {
    solve_wrn$condense_intertemporal
  } else if (matrix_method %in% c("SBBD", "DBBD", "NDBBD")) {
    solve_wrn$condense_bordered
  }
  if (is.null(msg)) {
    return(invisible(NULL))
  }
  if (matrix_method %in% c("SBBD", "NDBBD")) {
    msg <- c(msg, solve_wrn$condense_memory)
  }
  .cli_action(msg,
    action = c("warn", rep("inform", length(msg) - 1L)),
    call = call
  )
  return(invisible(NULL))
}
