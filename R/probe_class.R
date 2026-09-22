#' @importFrom cli cli_rule cli_text cli_alert_success cli_alert_danger
#' @importFrom utils head
#' @export
print.teems_probe <- function(x, ...) {
  cli::cli_rule(probe_info$print$rule)
  cli::cli_text(probe_info$print$system)
  for (pattern in c("structural", "realized")) {
    p <- x[[pattern]]
    if (is.null(p)) {
      next
    }
    lbl <- if (pattern %=% "structural") {
      probe_info$print$pattern_structural
    } else {
      probe_info$print$pattern_realized
    }
    if (!p$defective) {
      cli::cli_alert_success(probe_info$print$rank_full)
    } else {
      cli::cli_alert_danger(probe_info$print$rank_singular)
      if (NROW(p$under_by_var)) {
        agg <- paste0(p$under_by_var$name, " ×", p$under_by_var$count)
        cli::cli_text(probe_info$print$under_by_var)
      }
      if (NROW(p$over_by_eq)) {
        agg <- paste0(p$over_by_eq$name, " ×", p$over_by_eq$count)
        cli::cli_text(probe_info$print$over_by_eq)
      }
      if (!is.null(p$dm)) {
        cli::cli_text(probe_info$print$dm_blocks)
      }
    }
  }
  if (!is.null(x$cores)) {
    cli::cli_text(probe_info$print$fine_dm)
    if (NROW(x$cores$top)) {
      eqs <- x$cores$top$eqs[[1]]
      preview <- utils::head(paste0(eqs$name, " ×", eqs$count), 6L)
      cli::cli_text(probe_info$print$largest_core)
    }
  }
  if (NROW(x$statements)) {
    cli::cli_text(probe_info$print$statements)
  }
  if (!is.null(x$structure)) {
    cli::cli_text(probe_info$print$ordering)
  }
  .probe_print_cndns(x$condense)
  .probe_print_recommendation(x$recommendation)
  invisible(x)
}

#' @importFrom cli cli_rule cli_text
#' @importFrom utils head
#' @export
summary.teems_probe <- function(object, ...) {
  print(object)
  cli::cli_rule()
  if (NROW(object$defects)) {
    cli::cli_text(probe_info$print$defects)
    print(object$defects, n = 20)
  }
  if (NROW(object$incidence)) {
    dense <- object$incidence[order(-object$incidence$weight), ]
    cli::cli_text(probe_info$print$incidences)
    print(utils::head(dense, 10))
  }
  invisible(object)
}

#' @title Plot a structural probe
#' @export
#' @description Visualizations of a [`ems_probe()`] result:
#'   * `"incidence"`: the equation-system structure — a spy-style
#'   matrix of equation statements (rows, declaration order) by
#'   referenced variables (columns, first-appearance order), shaded by
#'   element-level incidence weight.
#'   * `"dm"`: the coarse Dulmage-Mendelsohn localization of a
#'   structurally singular system — the under-, well- and
#'   over-determined blocks to scale (only available when the probe
#'   found defects).
#'   * `"cores"`: the fine-decomposition core structure — sizes of the
#'   irreducible simultaneous cores and the composition of the largest
#'   core by equation statement (requires `fine = TRUE`).
#' @param x A `teems_probe` object from [`ems_probe()`].
#' @param type Character length 1: `"incidence"` (default), `"dm"`, or
#'   `"cores"`.
#' @param max_labels Integer length 1 (default `60L`). Axis labels are
#'   dropped when a dimension exceeds this count.
#' @param ... Unused.
#' @return The input, invisibly. Base-graphics side effects; the
#'   underlying tibbles (`x$incidence`, `x$cores`, pattern blocks) are
#'   exposed for custom plotting.
plot.teems_probe <- function(x,
                             type = c("incidence", "dm", "cores"),
                             max_labels = 60L,
                             ...) {
  type <- match.arg(type)
  switch(type,
    "incidence" = .probe_plot_incidence(x, max_labels = max_labels),
    "dm" = .probe_plot_dm(x),
    "cores" = .probe_plot_cores(x)
  )
  invisible(x)
}
