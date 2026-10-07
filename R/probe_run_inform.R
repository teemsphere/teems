#' @keywords internal
#' @noRd
.probe_elapsed_txt <- function(seconds) {
  txt <- if (seconds < 60) {
    sprintf("%.1f s", seconds)
  } else if (seconds < 3600) {
    sprintf("%dm %02ds", floor(seconds / 60), floor(seconds %% 60))
  } else {
    sprintf("%dh %02dm", floor(seconds / 3600), floor((seconds %% 3600) / 60))
  }
  return(txt)
}

#' @importFrom utils head
#' @keywords internal
#' @noRd
.probe_inform_warnings <- function(paths,
                                   call) {
  if (!file.exists(paths$diag_out)) {
    return(invisible(NULL))
  }
  log <- readLines(paths$diag_out, warn = FALSE)
  warn_lines <- unique(sub("^\\s*Warning:\\s*", "", grep("^\\s*Warning:", log, value = TRUE)))
  if (!length(warn_lines)) {
    return(invisible(NULL))
  }
  n_warn <- length(warn_lines)
  shown <- gsub("}", "}}", gsub("{", "{{", utils::head(warn_lines, 10L), fixed = TRUE), fixed = TRUE)
  if (n_warn > 10L) {
    shown <- c(shown, sprintf(probe_info$run$warn_more, n_warn - 10L))
  }
  .cli_action(c(probe_info$run$warnings, shown),
    action = rep("inform", length(shown) + 1L),
    call = call
  )
  return(invisible(NULL))
}
