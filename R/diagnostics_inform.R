#' @importFrom purrr map_chr
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
      \(x) {
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
                                 resources_record = NULL) {
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
      if (is.null(opt$ma48u)) {
        solve_info$record$ma48u_default
      } else {
        opt$ma48u
      }
    ),
    .resources_record_lines(resources_record),
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
      .onoff(opt$postsim), .onoff(opt$gpzerodivide)
    ),
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
