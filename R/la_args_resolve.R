#' MA48 workspace (la*) initial guesses: a user-passed value wins;
#' otherwise warm-start from the previous run's recorded `la_used`
#' (sol.stats.json under the same fixed convention as ems_compose);
#' otherwise the package cold defaults. Solver images with the
#' grow-and-retry paths treat these as starting sizes only, so an
#' undershot guess costs a redone analyse, not a failed run.
#'
#' @importFrom jsonlite read_json
#'
#' @keywords internal
#' @noRd
.resolve_la_args <- function(laA, laD, laDi, cmf_path) {
  cold <- list(laA = 300L, laD = 200L, laDi = 500L)
  used <- NULL
  stats_path <- file.path(
    dirname(cmf_path), "out", "variables", "bin", "sol.stats.json"
  )
  if (file.exists(stats_path)) {
    stats <- tryCatch(
      jsonlite::read_json(stats_path, simplifyVector = TRUE),
      error = \(e) NULL
    )
    # a structural probe run (ems_probe(), the recommendation's
    # probe) writes the same stats.json but never factorizes: its
    # la_used is the launch default, not a measurement -- ignore it
    if (!identical(stats$solution_method, "probe")) {
      used <- stats$la_used
    }
  }
  pick <- function(user, nm) {
    if (!is.null(user)) {
      value <- as.integer(user)
      return(value)
    }
    w <- used[[nm]]
    if (!is.null(w) && is.numeric(w) && length(w) == 1L && !is.na(w) && w > 0) {
      value <- as.integer(w)
      return(value)
    }
    return(cold[[nm]])
  }
  la_args <- list(
    laA = pick(laA, "laA"),
    laD = pick(laD, "laD"),
    laDi = pick(laDi, "laDi")
  )
  return(la_args)
}
