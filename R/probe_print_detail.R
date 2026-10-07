#' @importFrom cli format_inline
#' @keywords internal
#' @noRd
.probe_structure_txt <- function(x) {
  m <- probe_info$print
  s <- x$structure
  chained <- identical(s$chain_source, "structural")
  chain_txt <- if (chained) {
    chain_set <- s$chain_set
    n_time <- s$ntime
    border_n <- s$netcut %|||% 0L
    border <- .fmt(border_n)
    cli::format_inline(m$chain)
  } else {
    m$no_chain
  }
  partitioned <- identical(s$partition_source, "structural") &&
    isTRUE((s$ndblock %|||% 0L) > 1L)
  partition_txt <- if (partitioned) {
    partition_set <- s$partition_set
    n_blocks <- s$ndblock
    border <- if (chained) {
      cand <- s$partition_auto
      if (!is.null(cand) && partition_set %in% cand$set) {
        cand$netcut[match(partition_set, cand$set)]
      } else {
        NULL
      }
    } else {
      s$netcut
    }
    border_txt <- if (is.null(border)) {
      ""
    } else {
      border_n <- border
      border <- .fmt(border_n)
      cli::format_inline(m$partition_border)
    }
    cli::format_inline(m$partition)
  } else {
    m$no_partition
  }
  txt <- list(chain_txt = chain_txt, partition_txt = partition_txt)
  return(txt)
}

#' @importFrom cli cli_rule cli_bullets qty
#' @importFrom utils head
#' @keywords internal
#' @noRd
.probe_print_detail <- function(x) {
  m <- probe_info$print
  cli::cli_rule(left = m$rule)
  n_fmt <- .fmt(x$vecsize)
  cn <- x$condense
  if (isTRUE(cn$condensed)) {
    n_backsolve <- cn$n_backsolve
    uncondensed_fmt <- .fmt(round(cn$uncondensed))
    cli::cli_bullets(c("*" = m$system_condensed))
  } else {
    cli::cli_bullets(c("*" = m$system))
  }
  for (pattern in c("structural", "realized")) {
    p <- x[[pattern]]
    if (is.null(p)) {
      next
    }
    rank_txt <- .fmt(p$rank)
    n_txt <- .fmt(p$n)
    lbl <- if (pattern %=% "structural") {
      m$pattern_structural
    } else {
      m$pattern_realized
    }
    if (!p$defective) {
      cli::cli_bullets(c("v" = m$rank_full))
    } else {
      cli::cli_bullets(c("x" = m$rank_singular))
      if (NROW(p$under_by_var)) {
        agg <- paste0(p$under_by_var$name, "\u00a0\u00d7", p$under_by_var$count)
        cli::cli_bullets(c(" " = m$under_by_var))
      }
      if (NROW(p$over_by_eq)) {
        agg <- paste0(p$over_by_eq$name, "\u00a0\u00d7", p$over_by_eq$count)
        cli::cli_bullets(c(" " = m$over_by_eq))
      }
      if (!is.null(p$dm)) {
        dm <- lapply(p$dm, .fmt)
        cli::cli_bullets(c(" " = m$dm_blocks))
      }
    }
  }
  if (!is.null(x$structure)) {
    txt <- .probe_structure_txt(x)
    chain_txt <- txt$chain_txt
    partition_txt <- txt$partition_txt
    cli::cli_bullets(c("*" = m$structure))
  }
  if (!is.null(x$cores)) {
    sq_txt <- .fmt(x$cores$sq_comps)
    gt1_txt <- .fmt(x$cores$cores_gt1)
    largest_txt <- .fmt(x$cores$largest)
    cli::cli_bullets(c("*" = m$fine_dm))
    if (NROW(x$cores$top)) {
      eqs <- x$cores$top$eqs[[1]]
      preview <- utils::head(paste0(eqs$name, "\u00a0\u00d7", .fmt(eqs$count)), 6L)
      n_more <- NROW(eqs) - length(preview)
      if (n_more > 0L) {
        preview <- c(preview, sprintf(m$core_more, n_more))
      }
      cli::cli_bullets(c("*" = m$largest_core))
    }
  }
  if (NROW(x$statements)) {
    n_stmt <- NROW(x$statements)
    n_inc <- .fmt(NROW(x$incidence))
    cli::cli_bullets(c("*" = m$statements))
  }
  return(invisible(x))
}
