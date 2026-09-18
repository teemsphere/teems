#' @description One-line evidence string for the auto message.
#' @keywords internal
#' @noRd
.auto_evidence <- function(d) {
  fmt <- function(x) {
    txt <- format(x, big.mark = ",", scientific = FALSE, trim = TRUE)
    return(txt)
  }
  pct <- function(x) {
    txt <- paste0(format(round(100 * x, 1), nsmall = 1, trim = TRUE), "%")
    return(txt)
  }
  size <- if (is.na(d$system_size)) {
    "unknown size"
  } else {
    paste(fmt(d$system_size), "equations")
  }
  if (isTRUE(d$condensed)) {
    size <- paste(size, "(condensed)")
  }
  if (!isTRUE(d$probed)) {
    evidence <- paste0(
      size, ", n_tasks ", d$n_tasks,
      "; structural probe skipped (",
      d$probe_skip %|||% "not a candidate",
      ")"
    )
    return(evidence)
  }
  chain <- if (isTRUE(d$chain)) {
    paste0("chain ", d$chain_set, " (", d$n_time, " blocks)")
  } else {
    "no chain"
  }
  part <- if (is.null(d$partition)) {
    paste0("no partition viable for ", d$n_tasks, " task(s)")
  } else {
    p <- d$partition
    paste0(
      "partition ", p$set, " (", p$n_blocks, " blocks, border ",
      if (is.na(p$border_share)) {
        "n/a"
      } else {
        pct(p$border_share)
      }, ")"
    )
  }
  ceil <- if (is.null(d$lu_ceiling)) {
    ""
  } else if (isTRUE(d$lu_excluded)) {
    paste0(
      ", LU excluded (projected MA48 workspace ", fmt(round(d$lu_ceiling$projected)),
      " > 32-bit ceiling ", fmt(d$lu_ceiling$ceiling), ")"
    )
  } else {
    paste0(", LU workspace ", pct(d$lu_ceiling$share), " of the 32-bit ceiling")
  }
  evidence <- paste0(size, ", ", chain, ", ", part, ", n_tasks ", d$n_tasks, ceil)
  return(evidence)
}
