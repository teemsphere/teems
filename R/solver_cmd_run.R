#' @keywords internal
#' @noRd
.run_solver_cmd <- function(cmd) {
  if (Sys.info()[["sysname"]] %=% "Windows") {
    captured <- character(0)
    captured <- system(cmd, intern = TRUE)
    if (.o_verbose()) {
      cat(captured, sep = "\n")
    }
  } else if (.o_verbose()) {
    system(cmd)
  } else {
    system(cmd,
      ignore.stdout = TRUE,
      ignore.stderr = TRUE
    )
  }
  return(invisible(NULL))
}
