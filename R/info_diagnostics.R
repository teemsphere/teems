#' @importFrom purrr map_chr
#' 
#' @keywords internal
#' @noRd
.inform_diagnostics <- function(elapsed_time,
                                model_log,
                                run_dir,
                                call) {
  diagnostic_file <- file.path(run_dir, "model_diagnostics.txt")

  cat("\n", append = TRUE, file = diagnostic_file)
  cat("-- Solver log --\n\n", append = TRUE, file = diagnostic_file)
  cat(paste(model_log, collapse = "\n"), "\n", append = TRUE, file = diagnostic_file)

  if (any(grepl(pattern = "Accurate", model_log))) {
    accuracy_output <- model_log[grep("Accurate", model_log)]

    all_digits <- as.numeric(trimws(purrr::map_chr(
      strsplit(accuracy_output, "digits|none"),
      function(x) {
        x[length(x)]
      }
    )))

    total_var <- sum(all_digits)
    a4digits <- sum(all_digits[1:3])

    accurate_4 <- a4digits / total_var
    accuracy <- sprintf("%.0f%%", accurate_4 * 100)
    a_threshold <- .o_accuracy_threshold()
    elapsed_time_raw <- elapsed_time[[3]]

    if (elapsed_time_raw < 60) {
      elapsed_time_fmt <- sprintf("%.2fs", elapsed_time_raw)
    } else if (elapsed_time_raw < 3600) {
      elapsed_time_fmt <- sprintf("%dm %02ds", floor(elapsed_time_raw / 60), floor(elapsed_time_raw %% 60))
    } else {
      elapsed_time_fmt <- sprintf("%dh %02dm", floor(elapsed_time_raw / 3600), floor((elapsed_time_raw %% 3600) / 60))
    }

    elapsed_time <- elapsed_time_fmt

    .cli_action(solve_info$elapsed_time,
      action = "inform"
    )

    below_threshold <- round(accurate_4, 2) < a_threshold
    a_threshold_fmt <- sprintf("%.0f%%", a_threshold * 100)

    if (below_threshold) {
      a_threshold <- a_threshold_fmt
      .cli_action(solve_wrn$accuracy,
        action = c("warn", "inform"),
        call = call
      )
    } else if (.o_verbose()) {
      .cli_action(solve_info$accuracy,
        action = "inform"
      )
    }

    cat(
      "\n-- Run summary --\n\n",
      sprintf("Elapsed time:       %s\n", elapsed_time_fmt),
      sprintf("Accuracy (4-digit): %s\n", accuracy),
      sprintf("Accuracy threshold: %s\n", a_threshold_fmt),
      append = TRUE, file = diagnostic_file, sep = ""
    )
  }

  return(invisible(NULL))
}