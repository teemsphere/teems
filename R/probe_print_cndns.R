#' @description Render the probe's condensation verdict. Kept beside the
#'   solve-time advisory so both sides of the 6.2 guidance read together.
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
      cli::cli_text(
        "condensation: {n_backsolve} backsolved variable{?s} ({share} of
        the uncondensed system), but the probe finds a {blocks}-block
        partition on {.val {set}} (border {border})"
      )
      cli::cli_text(
        "  substitution densifies those blocks -- redeploy without
        {.arg backsolve} and solve with a bordered method"
      )
    },
    "helps" = {
      cli::cli_text(
        "condensation: {n_backsolve} backsolved variable{?s} ({share} of
        the uncondensed system); no usable block partition, so this
        system is {.val LU}-bound -- the case condensation pays for"
      )
    },
    "candidate" = {
      cli::cli_text(
        "condensation: none, and no usable block partition -- this
        {.val LU}-bound system is a candidate for
        {.fn ems_model} {.arg backsolve}"
      )
    }
  )
  return(invisible(NULL))
}
