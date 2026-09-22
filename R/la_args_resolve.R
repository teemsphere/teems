#' @keywords internal
#' @noRd
.pick <- function(user, nm, cold, used) {
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

#' @importFrom jsonlite read_json
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
    if (!identical(stats$solution_method, "probe")) {
      used <- stats$la_used
    }
  }
  la_args <- list(
    laA = .pick(laA, "laA", cold, used),
    laD = .pick(laD, "laD", cold, used),
    laDi = .pick(laDi, "laDi", cold, used)
  )
  return(la_args)
}
