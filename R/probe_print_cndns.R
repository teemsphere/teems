#' @importFrom cli cli_text
#' @keywords internal
#' @noRd
.probe_print_cndns <- function(condense) {
  if (is.null(condense) || condense$verdict %=% "none") {
    return(invisible(NULL))
  }
  share <- .cndns_share(condense)
  blocks <- condense$n_blocks
  set <- condense$partition_set %|||% (condense$chain_set %|||% "-")
  border <- condense$border
  n_backsolve <- condense$n_backsolve
  switch(condense$verdict,
    "hurts" = {
      cli::cli_text(probe_info$cndns$hurts)
      cli::cli_text(probe_info$cndns$hurts_advice)
    },
    "helps" = {
      cli::cli_text(probe_info$cndns$helps)
    },
    "candidate" = {
      cli::cli_text(probe_info$cndns$candidate)
    }
  )
  return(invisible(NULL))
}
