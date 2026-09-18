#' @keywords internal
#' @noRd
.probe_print_recommendation <- function(r) {
  if (is.null(r)) {
    return(invisible(NULL))
  }
  fmt_gb <- function(x) {
    if (is.null(x) || is.na(x)) {
      return("unknown")
    }
    if (x < 0.95) {
      txt <- paste0(format(round(x, 2), nsmall = 2, trim = TRUE), " GB")
      return(txt)
    }
    txt <- paste0(format(round(x, 1), nsmall = 1, trim = TRUE), " GB")
    return(txt)
  }
  n_tasks <- r$n_tasks
  n_threads <- r$n_threads
  if (is.null(r$host$cores) || is.na(r$host$cores)) {
    cli::cli_text("recommended (container not inspected, so one task and one thread): {.val {r$matrix_method}}")
  } else {
    cores <- r$host$cores
    mem <- fmt_gb(r$host$mem_gb)
    where <- if (identical(r$host$source, "given")) {
      "as given"
    } else {
      "this container"
    }
    cli::cli_text("recommended for {cores} core{?s}, {mem} ({where}): {.val {r$matrix_method}}, {n_tasks} task{?s} x {n_threads} thread{?s}")
  }
  cli::cli_text("  evidence: {r$evidence}")
  cli::cli_text("  {r$rationale}")
  if (!identical(r$method_johansen, r$matrix_method)) {
    cli::cli_text("  under Johansen the crossover differs: {.val {r$method_johansen}}")
  }
  d <- r$decision
  if (isTRUE(d$memory_arm)) {
    cli::cli_text("  {.val SBBD} does not fit the memory ({fmt_gb(d$memory$estimates$SBBD)} estimated); {.val NDBBD} holds the tables once")
  }
  if (isTRUE(d$dbbd_memory_blocked)) {
    cli::cli_text("  {.val DBBD} would be faster but its {fmt_gb(d$memory$estimates$DBBD)} estimate does not fit; {.val LU} stays")
  }
  if (isTRUE(d$lu_excluded)) {
    cli::cli_text("  {.val LU} excluded: the projected MA48 workspace passes the 32-bit ceiling")
  }
  fit <- r$fit
  if (!is.null(fit) && !is.na(fit$est_gb)) {
    cli::cli_text(
      "  memory: about {fmt_gb(fit$est_gb)} at {fit$n_tasks} task{?s}{if (is.na(fit$share)) '' else paste0(', ', round(100 * fit$share), '% of ', fmt_gb(fit$limit_gb))} -- {fit$verdict}"
    )
  }
  if (!is.null(r$tempdir)) {
    cli::cli_text("  scratch inside the container ({.path {r$tempdir}}); ems_solve() sets it for {.val NDBBD}")
  }
  cli::cli_text("  {.code {r$call}}")
  return(invisible(NULL))
}
