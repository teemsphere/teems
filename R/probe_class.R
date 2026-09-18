#' @importFrom cli cli_rule cli_text cli_alert_success cli_alert_danger
#'
#' @export
print.teems_probe <- function(x, ...) {
  cli::cli_rule("teems structural probe")
  cli::cli_text("condensed system: {x$vecsize} x {x$vecsize}")
  for (pattern in c("structural", "realized")) {
    p <- x[[pattern]]
    if (is.null(p)) {
      next
    }
    lbl <- if (pattern %=% "structural") {
      "structural pattern"
    } else {
      "realized pattern (nonzero at base data)"
    }
    if (!p$defective) {
      cli::cli_alert_success("{lbl}: full structural rank {p$rank} of {p$n}")
    } else {
      cli::cli_alert_danger(
        "{lbl}: structurally singular — rank {p$rank} of {p$n}
        ({p$unmatched_rows} unmatched equation{?s},
        {p$unmatched_cols} unmatched variable{?s})"
      )
      if (NROW(p$under_by_var)) {
        agg <- paste0(p$under_by_var$name, " ×", p$under_by_var$count)
        cli::cli_text("  under-determined by variable: {.val {agg}}")
      }
      if (NROW(p$over_by_eq)) {
        agg <- paste0(p$over_by_eq$name, " ×", p$over_by_eq$count)
        cli::cli_text("  over-constrained by equation: {.val {agg}}")
      }
      if (!is.null(p$dm)) {
        cli::cli_text(
          "  DM blocks: under {p$dm$m1} x {p$dm$n1},
          well {p$dm$m2} x {p$dm$n2}, over {p$dm$m3} x {p$dm$n3}"
        )
      }
    }
  }
  if (!is.null(x$cores)) {
    cli::cli_text(
      "fine DM: {x$cores$sq_comps} strongly connected component{?s} —
      {x$cores$cores_gt1} simultaneous core{?s} (>1 element),
      largest {x$cores$largest}"
    )
    if (NROW(x$cores$top)) {
      eqs <- x$cores$top$eqs[[1]]
      preview <- utils::head(paste0(eqs$name, " ×", eqs$count), 6L)
      cli::cli_text("  largest core by equation: {.val {preview}}")
    }
  }
  if (NROW(x$statements)) {
    cli::cli_text(
      "{NROW(x$statements)} equation statement{?s},
      {NROW(x$incidence)} statement-variable incidence{?s}"
    )
  }
  if (!is.null(x$structure)) {
    cli::cli_text(
      "ordering evidence: chain {x$structure$chain_source %|||% 'none'},
      partition {x$structure$partition_source %|||% 'none'}"
    )
  }
  .probe_print_cndns(x$condense)
  .probe_print_recommendation(x$recommendation)
  invisible(x)
}

#' @export
summary.teems_probe <- function(object, ...) {
  print(object)
  cli::cli_rule()
  if (NROW(object$defects)) {
    cli::cli_text("defect elements (capped upstream at 200 per list):")
    print(object$defects, n = 20)
  }
  if (NROW(object$incidence)) {
    dense <- object$incidence[order(-object$incidence$weight), ]
    cli::cli_text("heaviest statement-variable incidences:")
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
