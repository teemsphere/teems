#' Container resources: cores and memory as the solver will see them
#'
#' The solver runs inside the teems image, so the numbers that matter
#' are the container's, not the host's: Docker Desktop hands the Linux
#' VM a fixed share of the laptop (its Resources setting), and a
#' `--cpuset`/`--memory` cap or a cgroup limit on a workstation can be
#' tighter than the machine. One throwaway container per image and
#' session reads them (`nproc`, the cgroup v2/v1 memory limit, and
#' `MemTotal`); the result is cached for the session.
#'
#' @return `NULL` when the inspection fails (no docker, image missing,
#'   unexpected output), else a list: `cores` (integer), `mem_gb`
#'   (decimal GB: the smaller of the cgroup limit and the VM's
#'   `MemTotal`), `mem_total_gb`, `cgroup_limit_gb` (`NA` when no
#'   limit is set) and `image`.
#' @keywords internal
#' @noRd
.host_cache <- new.env(parent = emptyenv())

#' @keywords internal
#' @noRd
.container_resources <- function(image,
                                 refresh = FALSE) {
  if (!refresh && !is.null(.host_cache[[image]])) {
    return(.host_cache[[image]])
  }
  script <- paste(
    "nproc;",
    "cat /sys/fs/cgroup/memory.max 2>/dev/null ||",
    "cat /sys/fs/cgroup/memory/memory.limit_in_bytes 2>/dev/null ||",
    "echo max;",
    "grep MemTotal /proc/meminfo"
  )
  out <- tryCatch(
    suppressWarnings(system2(
      "docker",
      c("run", "--rm", "--entrypoint", "sh", image, "-c", .shell_quote(script)),
      stdout = TRUE,
      stderr = FALSE
    )),
    error = function(e) character()
  )
  host <- .parse_container_resources(out)
  if (!is.null(host)) {
    host$image <- image
    .host_cache[[image]] <- host
  }
  host
}

#' @description Pure parser of the three inspection lines, unit tested
#'   on canned output. cgroup v2 reports `max` when unlimited, v1 a
#'   huge number (2^63-ish); both are read as "no limit".
#' @keywords internal
#' @noRd
.parse_container_resources <- function(out) {
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
  mem <- if (unlimited) mem_total else min(mem_total, limit)
  list(
    cores = cores,
    mem_gb = mem / 1e9,
    mem_total_gb = mem_total / 1e9,
    cgroup_limit_gb = if (unlimited) NA_real_ else limit / 1e9
  )
}
