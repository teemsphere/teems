#' @importFrom grDevices gray.colors
#' @importFrom graphics mtext image
#' @keywords internal
#' @noRd
.probe_plot_incidence <- function(x,
                                  max_labels = 60L) {
  inc <- x$incidence
  if (!NROW(inc)) {
    .cli_action(
      msg = "This probe carries no statement incidence data (report
      version {x$version %|||% 1}); rerun against a solver image with
      probe report version 2.",
      action = "abort"
    )
  }
  eqs <- unique(inc$eq)
  vars <- unique(inc$var)
  m <- matrix(NA_real_, nrow = length(eqs), ncol = length(vars))
  m[cbind(
    match(inc$eq, eqs),
    match(inc$var, vars)
  )] <- log1p(inc$weight)

  op <- graphics::par(mar = c(3, 8, 8, 1))
  on.exit(graphics::par(op), add = TRUE)
  graphics::image(
    x = seq_along(vars),
    y = seq_along(eqs),
    z = t(m[rev(seq_along(eqs)), , drop = FALSE]),
    col = rev(grDevices::gray.colors(64, start = 0.05, end = 0.92)),
    axes = FALSE,
    xlab = "",
    ylab = ""
  )
  graphics::box()
  if (length(vars) <= max_labels) {
    graphics::axis(3,
      at = seq_along(vars), labels = vars,
      las = 2, cex.axis = 0.55, tick = FALSE, line = -0.5
    )
  }
  if (length(eqs) <= max_labels) {
    graphics::axis(2,
      at = seq_along(eqs), labels = rev(eqs),
      las = 2, cex.axis = 0.55, tick = FALSE, line = -0.5
    )
  }
  graphics::mtext(
    sprintf(
      "equation-system structure: %d statements x %d variables (%d incidences)",
      length(eqs), length(vars), NROW(inc)
    ),
    side = 1, line = 1, cex = 0.8
  )
  return(invisible(NULL))
}
