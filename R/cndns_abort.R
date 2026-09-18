# Deploy-time guard: swaps and shocks must not touch condensed variables.
#' @keywords internal
#' @noRd
.abort_cndnsd <- function(var_name,
                          var_extract,
                          err,
                          call) {
  r_idx <- match(tolower(var_name), tolower(var_extract$name))
  if (is.na(r_idx)) {
    return(invisible(NULL))
  }
  condense_action <- var_extract$condense[[r_idx]]
  if (!is.na(condense_action)) {
    .cli_action(err,
      action = c("abort", "inform"),
      call = call
    )
  }
  return(invisible(NULL))
}
