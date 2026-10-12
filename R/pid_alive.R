#' @keywords internal
#' @noRd
.pid_alive <- function(pid,
                       sysname = Sys.info()[["sysname"]]) {
  pid <- as.integer(pid)
  if (sysname %=% "Windows") {
    out <- tryCatch(
      suppressWarnings(system2("tasklist",
        c("/FI", shQuote(paste("PID eq", pid), type = "cmd")),
        stdout = TRUE, stderr = TRUE
      )),
      error = \(e) character()
    )
    alive <- any(grepl(paste0("\\b", pid, "\\b"), out))
  } else {
    status <- tryCatch(
      suppressWarnings(system2("ps", c("-p", pid), stdout = FALSE, stderr = FALSE)),
      error = \(e) 1L
    )
    alive <- identical(as.integer(status), 0L)
  }
  return(alive)
}
