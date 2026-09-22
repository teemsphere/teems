#' @keywords internal
#' @noRd
.host_cache <- new.env(parent = emptyenv())

#' @keywords internal
#' @noRd
.cntnr_resources <- function(image,
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
    error = \(e) character()
  )
  host <- .parse_cntnr_resources(out)
  if (!is.null(host)) {
    host$image <- image
    .host_cache[[image]] <- host
  }
  return(host)
}
