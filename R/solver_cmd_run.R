#' @keywords internal
#' @noRd
.run_solver_cmd <- function(cmd) {
  if (Sys.info()[["sysname"]] %=% "Windows") {
    captured <- suppressWarnings(system(cmd, intern = TRUE))
    status <- attr(captured, "status") %|||% 0L
  } else {
    status <- system(cmd,
      ignore.stdout = TRUE,
      ignore.stderr = TRUE
    )
  }
  return(invisible(status))
}
