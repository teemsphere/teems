#' @importFrom jsonlite read_json
#' @importFrom utils head
#' @keywords internal
#' @noRd
.inform_diagnostics <- function(elapsed_time,
                                model_log,
                                run_dir,
                                call) {
  diagnostic_file <- file.path(run_dir, "model_diagnostics.txt")

  cat("\n", append = TRUE, file = diagnostic_file)
  cat("-- Solver log --\n\n", append = TRUE, file = diagnostic_file)
  cat(paste(.solver_log_format(model_log), collapse = "\n"), "\n", append = TRUE, file = diagnostic_file)

  n_zdiv <- sum(grepl("took the Zerodivide default", model_log, fixed = TRUE))
  if (n_zdiv > 0L) {
    .cli_action(solve_info$zerodivide,
      action = "inform"
    )
  }

  stats_path <- file.path(run_dir, "out", "variables", "bin", "sol.stats.json")
  stats <- if (file.exists(stats_path)) {
    tryCatch(
      jsonlite::read_json(stats_path, simplifyVector = TRUE),
      error = \(e) NULL
    )
  }
  acc <- stats$accuracy
  if (is.null(acc)) {
    return(invisible(NULL))
  }

  linear <- as.numeric(acc$linear$figures)
  levels <- as.numeric(acc$levels$figures)
  judged_zero <- as.numeric(acc$linear$judged_zero)
  summarized <- as.numeric(acc$summarised)
  data_acc <- acc$data
  has_data <- isTRUE(as.numeric(data_acc$all$values) > 0)

  figures <- .o_accuracy_figures()
  criterion <- .o_accuracy_criterion()
  judged <- if (!has_data) "solution" else criterion
  above <- seq.int(figures + 1L, 7L)
  solution_count <- c(sum(levels[above]), summarized)
  data_count <- if (has_data) {
    c(
      sum(as.numeric(data_acc$all$figures)[above]) + as.numeric(data_acc$all$judged_zero),
      as.numeric(data_acc$all$values)
    )
  } else {
    c(0, 0)
  }
  counted <- switch(judged,
    solution = solution_count,
    data = data_count,
    both = solution_count + data_count
  )
  accurate_share <- if (counted[2] > 0) counted[1] / counted[2] else 1
  accuracy <- sprintf("%.0f%%", accurate_share * 100)
  selected <- acc$covers %=% "xac-retained"
  covers <- solve_info$accuracy_covers[[if (selected && judged != "data") paste0(judged, "_selected") else judged]]
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

  below_threshold <- round(accurate_share, 2) < a_threshold
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
  least_accurate <- acc$least_accurate
  has_least <- is.data.frame(least_accurate) && nrow(least_accurate) > 0L
  if (below_threshold && has_least) {
    shown <- utils::head(least_accurate, 5L)
    least <- paste(sprintf("%s (%d levels figure%s, %s)", shown$result, as.integer(shown$levels_figures),
      ifelse(as.integer(shown$levels_figures) == 1L, "", "s"), shown$code), collapse = ", ")
    .cli_action(solve_info$accuracy_least,
      action = "inform"
    )
  }

  criterion_note <- if (judged %=% criterion) criterion else sprintf("%s (no updated data; judged on the solution)", criterion)
  figures_table <- sprintf("%7d  %7.0f  %13.0f", 6:0, rev(linear), rev(levels))
  cat(
    "\n-- Run summary --\n\n",
    sprintf("Elapsed time:       %s\n", elapsed_time_fmt),
    sprintf("Accuracy (%s, %d figures): %s\n", judged, figures, accuracy),
    sprintf("Accuracy criterion: %s\n", criterion_note),
    sprintf("Accuracy threshold: %s\n", a_threshold_fmt),
    sprintf("Results summarized: %.0f (%s)\n", summarized, if (selected) paste("variables", paste(unlist(acc$variables), collapse = ", ")) else "endogenous and backsolved"),
    sprintf("Subintervals:       %.0f\n", as.numeric(acc$subintervals)),
    "figures  results  levels values\n",
    paste0(figures_table, "\n"),
    sprintf("Judged zero:        %.0f\n", judged_zero),
    append = TRUE, file = diagnostic_file, sep = ""
  )
  if (!is.null(acc$order)) {
    order_counts <- vapply(c("within", "lower", "higher", "oscillating", "not_measured"), \(k) as.numeric(acc$order[[k]] %|||% 0), numeric(1))
    cat(
      sprintf(
        "\nObserved order of convergence (expected %.0f): %.0f within 0.5 of it, %.0f lower, %.0f higher, %.0f oscillating, %.0f not measured\n",
        as.numeric(acc$expected_order %|||% NA), order_counts[1], order_counts[2], order_counts[3], order_counts[4], order_counts[5]
      ),
      append = TRUE, file = diagnostic_file, sep = ""
    )
  }
  if (has_least) {
    cat(
      "\nLeast accurate results (levels figures, figures, code, value, error):\n",
      paste0(sprintf("  %s  %d  %d  %s  %.7g  %.4g", least_accurate$result, as.integer(least_accurate$levels_figures),
        as.integer(least_accurate$figures), least_accurate$code, as.numeric(least_accurate$value), as.numeric(least_accurate$error)), "\n"),
      append = TRUE, file = diagnostic_file, sep = ""
    )
  }
  codes <- acc$codes
  if (length(codes) > 0L) {
    code_rows <- vapply(names(codes), \(nm) {
      lowest <- as.numeric(unlist(codes[[nm]]$lowest_figures))
      lowest_txt <- if (length(lowest) == 0L || is.na(lowest[1])) "" else sprintf("%14.0f", lowest[1])
      row <- sprintf("%-4s  %7.0f  %s", nm, as.numeric(codes[[nm]]$results), lowest_txt)
      return(trimws(row, which = "right"))
    }, character(1))
    cat(
      if (!is.null(acc$required_figures)) sprintf("\nRequired figures (XRA): %.0f\n", as.numeric(acc$required_figures)),
      "\ncode  results  lowest figures\n",
      paste0(code_rows, "\n"),
      append = TRUE, file = diagnostic_file, sep = ""
    )
  }
  if (has_data) {
    data_files <- data_acc$files
    file_figures <- data_files$figures
    data_cols <- lapply(seq_len(nrow(data_files)), \(i) {
      row <- if (is.matrix(file_figures)) file_figures[i, ] else file_figures[[i]]
      as.numeric(row)
    })
    names(data_cols) <- data_files$name
    if (length(data_cols) > 1L) {
      data_cols[["all files"]] <- as.numeric(data_acc$all$figures)
    }
    data_zero <- c(as.numeric(data_files$judged_zero), if (length(data_cols) > nrow(data_files)) as.numeric(data_acc$all$judged_zero))
    data_rows <- vapply(7:1, \(k) {
      paste(c(sprintf("%7d", k - 1L), sprintf("%12.0f", vapply(data_cols, `[`, numeric(1), k))), collapse = "  ")
    }, character(1))
    cat(
      sprintf("\nUpdated data values: %.0f (read from data files and updated)\n", as.numeric(data_acc$all$values)),
      paste(c("figures", sprintf("%12s", names(data_cols))), collapse = "  "), "\n",
      paste0(data_rows, "\n"),
      sprintf("Data judged zero:   %s\n", paste(sprintf("%.0f", data_zero), collapse = "  ")),
      append = TRUE, file = diagnostic_file, sep = ""
    )
  }

  return(invisible(NULL))
}
#' @keywords internal
#' @noRd
.onoff <- function(x) {
  txt <- ifelse(isTRUE(x), "on", "off")
  return(txt)
}

#' @importFrom jsonlite read_json
#' @keywords internal
#' @noRd
.solve_record_append <- function(run_dir,
                                 resources_record = NULL,
                                 cmf = NULL) {
  diagnostic_file <- file.path(run_dir, "model_diagnostics.txt")
  stats_path <- file.path(run_dir, "out", "variables", "bin", "sol.stats.json")
  if (!file.exists(diagnostic_file) || !file.exists(stats_path)) {
    return(invisible(NULL))
  }
  stats <- tryCatch(
    jsonlite::read_json(stats_path, simplifyVector = TRUE),
    error = \(e) NULL
  )
  opt <- stats$options
  if (is.null(opt)) {
    return(invisible(NULL))
  }
  lines <- c(
    "",
    sprintf("-- Solve record (%s) --", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")),
    "",
    if (!is.null(stats$solver_version)) {
      sprintf(solve_info$record$solver_version, stats$solver_version)
    },
    if (!is.null(opt$blas_core)) {
      sprintf(solve_info$record$blas, opt$blas_core)
    },
    sprintf(
      solve_info$record$method,
      stats$solution_method,
      if (!is.null(opt$steps)) {
        sprintf(solve_info$record$method_steps, paste(opt$steps, collapse = ", "))
      } else {
        ""
      },
      opt$subintervals
    ),
    if (!is.null(opt$adaptive)) {
      sprintf(solve_info$record$adaptive, opt$adaptive, opt$eps_tolerance)
    },
    if (!is.null(opt$rk_chart)) {
      sprintf(
        solve_info$record$rk_chart,
        opt$rk_chart, opt$rk_norm,
        if (identical(opt$rk_scope, "all")) {
          solve_info$record$rk_scope_all
        } else {
          solve_info$record$rk_scope_pc
        },
        opt$rk_controller,
        if (!is.null(opt$rk_h0) && opt$rk_h0 > 0) {
          sprintf(solve_info$record$rk_h0, opt$rk_h0)
        } else {
          ""
        }
      )
    },
    if (!is.null(stats$runge_kutta)) {
      rk <- stats$runge_kutta
      c(
        sprintf(
          solve_info$record$rk_run,
          rk$steps, rk$stage_solves, rk$stage_solves_reused,
          rk$rejects_accuracy, rk$rejects_crossed, rk$rejects_range,
          rk$rejects_assertion, rk$rejects_guard,
          if (is.null(rk$rejects_singular)) {
            0L
          } else {
            rk$rejects_singular
          },
          format(rk$h_min, digits = 3), format(rk$h_max, digits = 3)
        ),
        if (!is.null(rk$worst_estimated_metric)) {
          sprintf(
            solve_info$record$rk_error,
            format(rk$worst_estimated_metric, digits = 3)
          )
        }
      )
    },
    sprintf(
      solve_info$record$matrix_method,
      stats$matrix_method, opt$laA, opt$laDi, opt$laD, .onoff(opt$fastrefac),
      if (is.null(opt$ma48_cntl2)) {
        solve_info$record$ma48_cntl2_default
      } else {
        opt$ma48_cntl2
      }
    ),
    .resources_record_lines(resources_record),
    if (!is.null(stats$refine)) {
      sprintf(
        solve_info$record$refine_run,
        stats$refine$solves,
        format(stats$refine$residual_ratio_before_max, digits = 3),
        format(stats$refine$residual_ratio_after_max, digits = 3)
      )
    } else if (stats$matrix_method %=% "DBBD" && isFALSE(opt$refine)) {
      solve_info$record$refine_skipped
    },
    if (!is.null(stats$la_used)) {
      sprintf(
        solve_info$record$la_used,
        stats$la_used$laA, stats$la_used$laDi, stats$la_used$laD
      )
    },
    if (!is.null(stats$condest)) {
      sprintf(
        solve_info$record$condest,
        format(stats$condest$kappa_w1_max, digits = 3),
        format(stats$condest$kappa_w2_max, digits = 3),
        format(stats$condest$omega_max, digits = 3),
        stats$condest$solves, stats$condest$zero_rhs_skips
      )
    },
    sprintf(
      solve_info$record$parallelism,
      stats$mpi_size, opt$max_threads
    ),
    .memory_record_lines(stats$rss_gb),
    if (!is.null(opt$store_precision)) {
      sprintf(solve_info$record$storage, opt$store_precision)
    },
    sprintf(
      solve_info$record$system,
      stats$vecsize, stats$nexo
    ),
    sprintf(
      solve_info$record$modes,
      opt$assertions, opt$range_test_initial, opt$range_test_updated,
      .onoff(opt$postsim)
    ),
    if (!is.null(opt$random_seed) && !is.null(cmf) && .solver_uses_random(list(cmf = cmf))) {
      sprintf(solve_info$record$random_seed, opt$random_seed)
    },
    if (!is.null(opt$complementarity)) {
      cp <- opt$complementarity
      sprintf(
        solve_info$record$complementarity,
        cp$active_components, cp$steps_approx_run,
        .onoff(cp$do_approx_run), .onoff(cp$redo_steps),
        cp$redo_step_min_fraction, .onoff(cp$do_acc_run),
        cp$state_bound_error
      )
    }
  )
  cat(paste(unlist(lines), collapse = "\n"), "\n",
    sep = "",
    append = TRUE, file = diagnostic_file
  )
  return(invisible(NULL))
}

#' @keywords internal
#' @noRd
.gb <- function(x) {
  txt <- format(round(as.numeric(x), 2), nsmall = 2)
  return(txt)
}

#' @keywords internal
#' @noRd
.memory_record_lines <- function(rss) {
  if (is.null(rss) || !is.list(rss)) {
    return(NULL)
  }
  peak <- rss$peak
  phases <- rss[setdiff(names(rss), "peak")]
  phase_txt <- vapply(
    names(phases),
    \(nm) {
      sprintf(solve_info$record$memory_phase, gsub("_", " ", nm, fixed = TRUE), .gb(phases[[nm]]$max), .gb(phases[[nm]]$sum))
    },
    character(1)
  )
  lines <- c(
    if (length(phase_txt) > 0L) {
      sprintf(
        solve_info$record$memory_phases,
        paste(phase_txt, collapse = "; ")
      )
    },
    if (!is.null(peak)) {
      sprintf(
        solve_info$record$memory_peak,
        .gb(peak$max), .gb(peak$sum)
      )
    }
  )
  return(lines)
}
