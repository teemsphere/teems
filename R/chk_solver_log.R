#' @keywords internal
#' @noRd
.map_solver_errors <- function(err_lines) {
  class <- rep(NA_character_, length(err_lines))
  manual <- rep(NA_character_, length(err_lines))
  for (i in seq_along(err_lines)) {
    hit <- which(vapply(
      solver_error_map$pattern,
      grepl,
      logical(1),
      x = err_lines[i],
      ignore.case = TRUE
    ))
    if (length(hit) > 0L) {
      class[i] <- solver_error_map$class[hit[1]]
      manual[i] <- solver_error_map$manual[hit[1]]
    }
  }
  errors <- data.frame(class = class, manual = manual, stringsAsFactors = FALSE)
  return(errors)
}

#' @importFrom utils head
#' @keywords internal
#' @noRd
.check_solver_log <- function(elapsed_time,
                              solve_cmd,
                              paths,
                              call,
                              status = 0L,
                              resources_record = NULL) {
  model_log <- readLines(paths$diag_out)
  diag_out <- normalizePath(paths$diag_out, "/")
  paths$diag_out <- diag_out

  condest_warn <- grep("condest: WARNING", model_log,
    value = TRUE, fixed = TRUE
  )
  scan_log <- model_log[
    !startsWith(model_log, "condest:") & !startsWith(model_log, "memory:")
  ]
  if (length(condest_warn) > 0L) {
    kappa_w2 <- sub(".*\\(kappa_w2 ([^)]*)\\).*", "\\1", condest_warn[1])
    .cli_action(solve_err$condest_nearsing,
      action = c("warn", "inform"),
      call = call
    )
  }

  err_lines <- grep("Error:", scan_log, value = TRUE, fixed = TRUE)
  if (length(err_lines) > 0L) {
    err_lines <- unique(sub(".*Error:\\s*", "", err_lines))
    mapped <- .map_solver_errors(err_lines)
    sel <- intersect(c("interface", "system", "tab", "closure", "subtotal", "data", "numeric", "resource", "size"), mapped$class)

    n_err <- length(err_lines)
    preview <- utils::head(err_lines, 10L)
    if (n_err > 10L) {
      preview <- c(preview, paste0("... and ", n_err - 10L, " more"))
    }
    err_preview <- paste(preview, collapse = "\f")

    if (length(sel) > 0L) {
      sel <- sel[1]
      manual_secs <- unique(mapped$manual[mapped$class == sel & !is.na(mapped$manual)])
      msg_name <- switch(sel,
        interface = "solver_interface",
        system = "solver_system",
        tab = "solver_tab",
        closure = "solver_closure",
        subtotal = "solver_subtotal",
        data = "solver_data",
        numeric = "solver_numeric",
        resource = "solver_resource",
        size = "solver_size"
      )
      msg <- solve_err[[msg_name]]
      action <- c("abort", rep("inform", length(msg) - 1L))
      if (sel %in% c("tab", "numeric") && length(manual_secs) == 0L) {
        msg <- msg[-3]
        action <- action[-3]
      }
      .cli_action(msg,
        action = action,
        call = call
      )
    }
    .cli_action(solve_err$solution_err,
      action = "abort",
      call = call
    )
  }

  fatal_log <- scan_log[!grepl("^\\s*Warning:", scan_log) &
    !grepl("^Step [0-9]+: .*, retrying with step size", scan_log)]
  if (any(grepl(pattern = "singular", fatal_log, ignore.case = TRUE))) {
    viol_lines <- grep("has an updated value", scan_log, value = TRUE, fixed = TRUE)
    if (length(viol_lines) > 0L) {
      n_viol <- length(viol_lines)
      first_viol <- sub(".*: coefficient ", "", viol_lines[1])
      .cli_action(solve_err$solution_sing_range,
        action = c("abort", "inform", "inform", "inform"),
        call = call
      )
    }
    .cli_action(solve_err$solution_sing,
      action = c("abort", "inform", "inform"),
      call = call
    )
  }
  if (any(grepl("error", fatal_log, ignore.case = TRUE))) {
    .cli_action(solve_err$solution_err,
      action = "abort",
      call = call
    )
  }
  if (!identical(as.integer(status), 0L)) {
    .cli_action(solve_err$solver_exit,
      action = "abort",
      call = call
    )
  }

  writeLines(solve_cmd, file.path(paths$run, "model_exec.txt"))
  .inform_diagnostics(
    elapsed_time = elapsed_time,
    model_log = model_log,
    run_dir = paths$run,
    call = call
  )
  .solve_record_append(
    run_dir = paths$run,
    resources_record = resources_record,
    cmf = paths$cmf
  )

  return(invisible(NULL))
}
