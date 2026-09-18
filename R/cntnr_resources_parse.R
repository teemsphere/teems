#' @description Pure parser of the three inspection lines, unit tested
#'   on canned output. cgroup v2 reports `max` when unlimited, v1 a
#'   huge number (2^63-ish); both are read as "no limit".
#' @keywords internal
#' @noRd
.parse_cntnr_resources <- function(out) {
  out <- out[nzchar(trimws(out %|||% character()))]
  if (length(out) < 3L) {
    return(NULL)
  }
  cores <- suppressWarnings(as.integer(trimws(out[1])))
  limit <- suppressWarnings(as.numeric(trimws(out[2])))
  mem_total_kb <- suppressWarnings(as.numeric(gsub("[^0-9]", "", out[3])))
  if (is.na(cores) || cores < 1L || is.na(mem_total_kb) || mem_total_kb <= 0) {
    return(NULL)
  }
  mem_total <- mem_total_kb * 1024
  unlimited <- is.na(limit) || limit >= 2^60
  mem <- if (unlimited) {
    mem_total
  } else {
    min(mem_total, limit)
  }
  resources <- list(
    cores = cores,
    mem_gb = mem / 1e9,
    mem_total_gb = mem_total / 1e9,
    cgroup_limit_gb = if (unlimited) {
      NA_real_
    } else {
      limit / 1e9
    }
  )
  return(resources)
}
