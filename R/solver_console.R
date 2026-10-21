#' @keywords internal
#' @noRd
.solver_console_state <- function() {
  state <- new.env(parent = emptyenv())
  state$block <- "none"
  state$blank <- FALSE
  state$kept <- FALSE
  state$started <- FALSE
  return(state)
}

#' @keywords internal
#' @noRd
.solver_block_header <- function(line) {
  header <- startsWith(line, "Accuracy summary") ||
    startsWith(line, "Overall accuracy summary") ||
    startsWith(line, "Updated data accuracy summary") ||
    startsWith(line, "Accuracy estimates")
  return(header)
}

#' @keywords internal
#' @noRd
.solver_console_keep <- function(line, state, verbosity) {
  if (nzchar(line) && !startsWith(line, " ")) {
    state$block <- if (startsWith(line, "Accuracy summary (") ||
      startsWith(line, "Overall accuracy summary")) {
      "summary"
    } else if (startsWith(line, "Accuracy summary for subinterval") ||
      startsWith(line, "Updated data accuracy summary")) {
      "dropped"
    } else {
      "none"
    }
  }
  if (state$block == "summary" && startsWith(line, "  code  meaning")) {
    state$block <- "codes"
  }
  if (state$block == "codes") {
    if (startsWith(line, "  observed order of convergence")) {
      state$block <- "dropped"
    }
    return(FALSE)
  }
  if (state$block == "dropped") {
    return(FALSE)
  }
  if (grepl("took the Zerodivide default", line, fixed = TRUE)) {
    return(FALSE)
  }
  if (grepl("^(Prepare|LU|Update|Last Update) time |^One step solution |^memory: |^Maximum residual ratio |^postsim: |^inmemory: |^Statement order: |^  judged zero ", line)) {
    return(FALSE)
  }
  if (verbosity == 0L) {
    keep <- state$block == "summary" ||
      startsWith(line, "Error") ||
      startsWith(line, "Warning") ||
      startsWith(line, "condest: WARNING") ||
      startsWith(line, "Accuracy estimates")
    return(keep)
  }
  return(TRUE)
}

#' @keywords internal
#' @noRd
.solver_console_lines <- function(lines, state, verbosity) {
  if (verbosity >= 2L) {
    return(lines)
  }
  out <- character(0)
  for (line in lines) {
    if (!nzchar(trimws(line))) {
      state$blank <- state$kept
      next
    }
    keep <- .solver_console_keep(line, state, verbosity)
    state$kept <- keep
    if (!keep) {
      next
    }
    if (state$started && (state$blank || .solver_block_header(line))) {
      out <- c(out, "")
    }
    state$blank <- FALSE
    state$started <- TRUE
    out <- c(out, line)
  }
  return(out)
}

#' @keywords internal
#' @noRd
.solver_console_cat <- function(lines, state, verbosity) {
  shown <- .solver_console_lines(lines, state, verbosity)
  if (length(shown) > 0L) {
    cat(shown, sep = "\n")
  }
  return(invisible(shown))
}

#' @keywords internal
#' @noRd
.solver_log_format <- function(lines) {
  out <- character(0)
  prev <- ""
  for (line in lines) {
    blank <- !nzchar(trimws(line))
    if (blank && grepl("took the Zerodivide default", prev, fixed = TRUE)) {
      next
    }
    if (!blank && length(out) > 0L && nzchar(prev) && .solver_block_header(line)) {
      out <- c(out, "")
    }
    out <- c(out, line)
    prev <- line
  }
  return(out)
}

#' @keywords internal
#' @noRd
.relay_solver_cmd <- function(cmd, verbosity, timeID) {
  show <- .o_verbose()
  state <- .solver_console_state()
  if (Sys.info()[["sysname"]] %=% "Windows") {
    captured <- suppressWarnings(system(cmd, intern = TRUE))
    if (show) {
      .solver_console_cat(captured, state, verbosity)
    }
    status <- attr(captured, "status") %|||% 0L
    return(invisible(status))
  }
  con <- pipe(cmd, open = "rt")
  interrupted <- FALSE
  tryCatch(
    repeat {
      line <- readLines(con, n = 1L, warn = FALSE)
      if (length(line) == 0L) {
        break
      }
      if (show) {
        .solver_console_cat(line, state, verbosity)
      }
    },
    interrupt = function(e) interrupted <<- TRUE
  )
  if (interrupted) {
    .container_remove(timeID)
  }
  status <- suppressWarnings(close(con)) %|||% 0L
  status <- as.integer(status) %/% 256L
  if (interrupted) {
    status <- 130L
  }
  return(invisible(status))
}
