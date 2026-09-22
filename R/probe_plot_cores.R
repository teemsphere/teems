#' @importFrom graphics par barplot title plot.new
#' @importFrom utils head
#' @keywords internal
#' @noRd
.probe_plot_cores <- function(x) {
  if (is.null(x$cores)) {
    .cli_action(probe_err$no_fine,
      action = "abort"
    )
  }
  op <- graphics::par(mfrow = c(1, 2), mar = c(5, 5, 3, 1))
  on.exit(graphics::par(op), add = TRUE)

  sizes <- x$cores$sizes
  nontriv <- sizes[sizes$size > 1, , drop = FALSE]
  singletons <- sum(sizes$count[sizes$size == 1])
  if (NROW(nontriv)) {
    heights <- rep(nontriv$size, nontriv$count)
    heights <- sort(heights, decreasing = TRUE)
    graphics::barplot(
      heights,
      log = if (max(heights) / max(min(heights), 1) > 50) {
        "y"
      } else {
        ""
      },
      col = "gray55", border = NA,
      xlab = probe_info$plot$cores_xlab, ylab = probe_info$plot$cores_ylab,
      main = sprintf(
        probe_info$plot$cores_main,
        sum(nontriv$count), singletons
      ),
      cex.main = 0.85
    )
  } else {
    graphics::plot.new()
    graphics::title(main = probe_info$plot$cores_none,
      cex.main = 0.85
    )
  }

  top <- x$cores$top
  if (NROW(top)) {
    eqs <- top$eqs[[1]]
    shown <- utils::head(eqs, 12L)
    other <- sum(eqs$count) - sum(shown$count)
    heights <- shown$count
    labels <- shown$name
    cols <- rep("gray35", length(heights))
    if (other > 0) {
      heights <- c(heights, other)
      labels <- c(labels, sprintf(probe_info$plot$core_other, NROW(eqs) - NROW(shown)))
      cols <- c(cols, "gray75")
    }
    graphics::barplot(
      rev(heights),
      names.arg = rev(labels),
      horiz = TRUE, las = 1, col = rev(cols), border = NA,
      cex.names = 0.65,
      xlab = probe_info$plot$core_xlab,
      main = sprintf(probe_info$plot$core_main, top$size[[1]]),
      cex.main = 0.85
    )
  } else {
    graphics::plot.new()
    graphics::title(main = probe_info$plot$core_none, cex.main = 0.85)
  }
  return(invisible(NULL))
}
