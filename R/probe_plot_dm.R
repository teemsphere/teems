#' @importFrom graphics rect box axis text
#' @keywords internal
#' @noRd
.probe_plot_dm <- function(x) {
  p <- x$structural
  if (is.null(p) || !p$defective || is.null(p$dm)) {
    p <- x$realized
  }
  if (is.null(p) || !p$defective || is.null(p$dm)) {
    .cli_action(probe_err$no_defects_dm,
      action = "abort"
    )
  }
  dm <- p$dm
  n <- p$n
  op <- graphics::par(mar = c(4, 4, 3, 1))
  on.exit(graphics::par(op), add = TRUE)
  graphics::plot(
    NA,
    xlim = c(0, n), ylim = c(0, n),
    xlab = "variables (columns)", ylab = "equations (rows)",
    axes = FALSE, asp = 1
  )
  graphics::axis(1)
  graphics::axis(2)
  graphics::box()
  # DM order: under-determined columns first, unmatched rows last;
  # rows are counted from the top (y flipped). Degenerate blocks (a few
  # rows/columns in a 10^4+ system) get a minimum visible marker size,
  # anchored at their true corner, with the label set beside them; the
  # well block is drawn first so the markers stay on top.
  min_px <- n / 50
  blocks <- list(
    list(
      x0 = dm$n1, y1 = n - dm$m1, w = dm$n2, h = dm$m2,
      col = "gray82", lab = "well", anchor = "left"
    ),
    list(
      x0 = 0, y1 = n, w = dm$n1, h = dm$m1,
      col = "#c9573b", lab = "under", anchor = "left"
    ),
    list(
      x0 = dm$n1 + dm$n2, y1 = n - dm$m1 - dm$m2, w = dm$n3, h = dm$m3,
      col = "#d99a3d", lab = "over", anchor = "right"
    )
  )
  for (b in blocks) {
    if (b$w <= 0 && b$h <= 0) {
      next
    }
    w_d <- max(b$w, min_px)
    h_d <- max(b$h, min_px)
    if (b$anchor %=% "right") {
      x1 <- min(b$x0 + b$w, n)
      x0 <- x1 - w_d
    } else {
      x0 <- b$x0
      x1 <- min(x0 + w_d, n)
    }
    y1 <- b$y1
    y0 <- max(y1 - h_d, 0)
    y1 <- min(y0 + h_d, n) # keep the marker its minimum size at the edges
    graphics::rect(x0, y0, x1, y1, col = b$col, border = "gray30")
    lab <- sprintf("%s %d x %d", b$lab, b$h, b$w)
    if (b$w > n / 8 && b$h > n / 8) {
      graphics::text((x0 + x1) / 2, (y0 + y1) / 2, lab, cex = 0.8)
    } else if (x1 < n / 2) {
      graphics::text(x1 + n / 90, (y0 + y1) / 2, lab, cex = 0.75, adj = 0)
    } else {
      graphics::text(x0 - n / 90, (y0 + y1) / 2, lab, cex = 0.75, adj = 1)
    }
  }
  graphics::title(
    main = sprintf(
      "Dulmage-Mendelsohn localization (%s pattern): rank %d of %d",
      if (identical(p, x$structural)) {
        "structural"
      } else {
        "realized"
      },
      p$rank, p$n
    ),
    cex.main = 0.9
  )
  return(invisible(NULL))
}
