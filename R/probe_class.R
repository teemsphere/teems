#' @importFrom cli cli_rule cli_bullets format_inline
#' @export
print.teems_probe <- function(x, ...) {
  b <- probe_info$brief
  n_fmt <- .fmt(x$vecsize)
  cli::cli_rule(left = b$rule, right = cli::format_inline(b$size))
  singular <- FALSE
  for (pattern in c("structural", "realized")) {
    p <- x[[pattern]]
    if (is.null(p) || !p$defective || singular) {
      next
    }
    singular <- TRUE
    rank_txt <- .fmt(p$rank)
    n_txt <- .fmt(p$n)
    msg <- if (pattern %=% "structural") b$singular else b$singular_base
    cli::cli_bullets(c("x" = msg))
    if (NROW(p$under_by_var)) {
      agg <- paste0(p$under_by_var$name, "\u00a0\u00d7", p$under_by_var$count)
      cli::cli_bullets(c(" " = b$under))
    }
    if (NROW(p$over_by_eq)) {
      agg <- paste0(p$over_by_eq$name, "\u00a0\u00d7", p$over_by_eq$count)
      cli::cli_bullets(c(" " = b$over))
    }
    cli::cli_bullets(c("i" = b$dm_hint))
  }
  if (!singular) {
    cli::cli_bullets(c("v" = b$valid))
  }
  cn <- x$condense
  if (!is.null(cn) && cn$verdict %=% "candidate") {
    cli::cli_bullets(c("i" = b$candidate))
  }
  if (!is.null(cn) && cn$verdict %=% "hurts") {
    set <- cn$partition_set %|||% (cn$chain_set %|||% "-")
    blocks <- cn$n_blocks
    cli::cli_bullets(c("!" = b$hurts))
  }
  .probe_print_recommendation(x$recommendation, brief = TRUE)
  cli::cli_bullets(c("i" = b$more))
  invisible(x)
}

#' @importFrom cli cli_rule
#' @importFrom utils head
#' @export
summary.teems_probe <- function(object, ...) {
  .probe_print_detail(object)
  if (NROW(object$defects)) {
    cli::cli_rule(left = probe_info$print$defects)
    print(object$defects, n = 20)
  }
  if (NROW(object$incidence)) {
    dense <- object$incidence[order(-object$incidence$weight), ]
    cli::cli_rule(left = probe_info$print$incidences)
    print(utils::head(dense, 10))
  }
  .probe_print_cndns(object$condense)
  .probe_print_recommendation(object$recommendation)
  if (!is.null(object$paths$cmf) && !is.null(object$paths$log)) {
    x <- object
    cli::cli_bullets(c("i" = probe_info$print$files))
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
