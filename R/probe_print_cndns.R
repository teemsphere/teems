#' @importFrom cli cli_rule cli_bullets
#' @keywords internal
#' @noRd
.probe_print_cndns <- function(condense) {
  if (is.null(condense) || condense$verdict %=% "none") {
    return(invisible(NULL))
  }
  m <- probe_info$cndns
  cli::cli_rule(left = m$rule)
  if (condense$condensed) {
    share <- .cndns_share(condense)
    n_backsolve <- condense$n_backsolve
    uncondensed_fmt <- .fmt(round(condense$uncondensed))
    n_fmt <- .fmt(round(condense$uncondensed - condense$n_backsolve_ele))
    cli::cli_bullets(c("*" = m$status))
  }
  blocks <- condense$n_blocks
  set <- condense$partition_set %|||% (condense$chain_set %|||% "-")
  border <- condense$border %|||% 0L
  lu_size <- .fmt(condense$lu_size)
  switch(condense$verdict,
    "faster" = cli::cli_bullets(c("v" = m$faster)),
    "lu_fine" = cli::cli_bullets(c("v" = m$lu_fine)),
    "slower" = cli::cli_bullets(c("!" = m$slower, "i" = m$slower_advice)),
    "candidate" = cli::cli_bullets(c("i" = m$candidate))
  )
  return(invisible(NULL))
}
