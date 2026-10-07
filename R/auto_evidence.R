#' @keywords internal
#' @noRd
.fmt <- function(x) {
  txt <- format(x, big.mark = ",", scientific = FALSE, trim = TRUE)
  return(txt)
}

#' @keywords internal
#' @noRd
.pct <- function(x) {
  txt <- paste0(format(round(100 * x, 1), nsmall = 1, trim = TRUE), "%")
  return(txt)
}

#' @keywords internal
#' @noRd
.auto_evidence <- function(d) {
  ev <- probe_info$evidence
  size <- if (is.na(d$system_size)) {
    ev$size_unknown
  } else {
    sprintf(ev$size, .fmt(d$system_size))
  }
  if (isTRUE(d$condensed)) {
    size <- sprintf(ev$condensed, size)
  }
  n_tasks <- as.integer(d$n_tasks)
  tasks_txt <- sprintf(if (n_tasks == 1L) ev$task else ev$tasks, n_tasks)
  if (!isTRUE(d$probed)) {
    evidence <- sprintf(ev$skipped, size, tasks_txt, d$probe_skip %|||% ev$skip_default)
    return(evidence)
  }
  chain <- if (isTRUE(d$chain)) {
    sprintf(ev$chain, d$chain_set, as.integer(d$n_time))
  } else {
    ev$no_chain
  }
  part <- if (is.null(d$partition)) {
    sprintf(ev$no_partition, tasks_txt)
  } else {
    p <- d$partition
    share <- if (is.na(p$border_share)) {
      ev$border_na
    } else {
      .pct(p$border_share)
    }
    sprintf(ev$partition, p$set, as.integer(p$n_blocks), share)
  }
  ceil <- if (is.null(d$lu_ceiling)) {
    ""
  } else if (isTRUE(d$lu_excluded)) {
    sprintf(ev$lu_excluded, .fmt(round(d$lu_ceiling$projected)), .fmt(d$lu_ceiling$ceiling))
  } else {
    sprintf(ev$lu_share, .pct(d$lu_ceiling$share))
  }
  evidence <- sprintf(ev$full, size, chain, part, tasks_txt, ceil)
  return(evidence)
}
