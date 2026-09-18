.chk_raw_reads <- function(statements,
                           call) {
  reads <- grep("^\\s*read\\b", statements, ignore.case = TRUE, value = TRUE)
  if (length(reads) == 0L) {
    return(invisible(NULL))
  }
  # specific defects first: terminal source, partial (indexed) reads,
  # missing file clause; only then the generic header requirement
  term <- grepl("from\\s+terminal", reads, ignore.case = TRUE)
  if (any(term)) {
    bad_stmt <- trimws(reads[term][1])
    .cli_action(model_err$read_terminal,
      action = "abort",
      call = call
    )
  }
  # (by_elements) mapping reads (manual 11.9.3) and (IfHeaderExists)
  # optional reads (manual 10.6) are legal; any other parenthesized
  # form is a partial (indexed) read
  reads_nq <- sub("^(\\s*read)\\s*\\(\\s*(by_elements|ifheaderexists)\\s*\\)", "\\1",
    reads,
    ignore.case = TRUE
  )
  if (any(grepl("\\(", reads_nq))) {
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
