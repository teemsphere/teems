#' @keywords internal
#' @noRd
.run_solver_cmd <- function(cmd) {
  if (Sys.info()[["sysname"]] %=% "Windows") {
    captured <- suppressWarnings(system(cmd, intern = TRUE))
    if (.o_verbose()) {
      cat(captured, sep = "\n")
    }
    status <- attr(captured, "status") %|||% 0L
  } else if (.o_verbose()) {
    status <- system(cmd)
  } else {
    status <- system(cmd,
      ignore.stdout = TRUE,
      ignore.stderr = TRUE
    )
  }
  return(invisible(status))
}
