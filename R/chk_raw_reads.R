.chk_raw_reads <- function(statements,
                           call) {
  reads <- grep("^\\s*read\\b", statements, ignore.case = TRUE, value = TRUE)
  if (length(reads) == 0L) {
    return(invisible(NULL))
  }
  term <- grepl("from\\s+terminal", reads, ignore.case = TRUE)
  if (any(term)) {
    bad_stmt <- trimws(reads[term][1])
    .cli_action(model_err$read_terminal,
      action = "abort",
      call = call
    )
  }
  reads_nq <- sub("^(\\s*read)\\s*\\(\\s*(by_elements|ifheaderexists)\\s*\\)", "\\1",
    reads,
    ignore.case = TRUE
  )
  maps <- grep("^\\s*mapping\\b", statements, ignore.case = TRUE, value = TRUE)
  maps <- tolower(sub("^\\s*mapping\\s*(\\([^)]*\\))?\\s*([^ (]+).*$", "\\2", maps, ignore.case = TRUE))
  part <- grepl("^\\s*read\\s*\\(\\s*all\\s*,", reads_nq, ignore.case = TRUE)
  part_tgt <- tolower(sub("^\\s*read\\s*\\([^)]*\\)\\s*([^ (]+).*$", "\\1", reads_nq, ignore.case = TRUE))
  decls <- grep("^\\s*coefficient\\b", statements, ignore.case = TRUE, value = TRUE)
  full <- part & .read_is_full(reads_nq, decls)
  reads_nq <- reads_nq[!(part & (part_tgt %in% maps | full))]
  if (any(grepl("\\(", reads_nq))) {
    bad_reads <- trimws(reads_nq[grepl("\\(", reads_nq)])
    .cli_action(model_err$invalid_read,
      action = "abort",
      call = call
    )
  }
  if (!all(grepl("from\\s+file", reads, ignore.case = TRUE))) {
    .cli_action(model_err$missing_file,
      action = "abort",
      call = call
    )
  }
  nohdr <- !grepl("\\bheader\\b", reads, ignore.case = TRUE)
  if (any(nohdr)) {
    bad_reads <- trimws(reads[nohdr])
    .cli_action(model_err$read_no_header,
      action = "abort",
      call = call
    )
  }
  return(invisible(NULL))
}

#' @keywords internal
#' @noRd
.read_is_full <- function(reads, decls) {
  decl_sets <- lapply(decls, \(d) .read_target_sets(sub("^\\s*coefficient\\s*", "", d, ignore.case = TRUE)))
  decl_names <- tolower(vapply(decl_sets, \(x) x$name %|||% "", character(1)))
  full <- vapply(reads, \(r) {
    rd <- .read_target_sets(sub("^\\s*read\\s*", "", r, ignore.case = TRUE))
    if (is.null(rd$name) || !identical(rd$args, rd$idx)) {
      return(FALSE)
    }
    hit <- match(tolower(rd$name), decl_names)
    if (is.na(hit)) {
      return(FALSE)
    }
    identical(tolower(rd$sets), tolower(decl_sets[[hit]]$sets))
  }, logical(1), USE.NAMES = FALSE)
  return(full)
}

#' @keywords internal
#' @noRd
.read_target_sets <- function(text) {
  text <- gsub("#[^#]*#", "", text)
  rest <- .split_tab_qualifiers(text)$rest
  quant <- .split_tab_quantifiers(rest)
  groups <- if (is.na(quant$full_set)) {
    character(0)
  } else {
    regmatches(quant$full_set, gregexpr("\\(all,[^,)]+,[^,)]+", quant$full_set, ignore.case = TRUE))[[1]]
  }
  idx <- tolower(sub("^\\(all,([^,)]+),.*$", "\\1", groups, ignore.case = TRUE))
  sets <- sub("^\\(all,[^,)]+,([^,)]+)$", "\\1", groups, ignore.case = TRUE)
  m <- regmatches(quant$rest, regexec("^([A-Za-z_][A-Za-z0-9_@]*)\\s*(\\(([^)]*)\\))?", quant$rest))[[1]]
  if (length(m) == 0L) {
    target <- list(name = NULL)
    return(target)
  }
  args <- if (nzchar(m[4])) tolower(trimws(strsplit(m[4], ",", fixed = TRUE)[[1]])) else character(0)
  target <- list(name = m[2], args = args, idx = idx, sets = sets)
  return(target)
}
