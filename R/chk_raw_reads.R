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
  reads_nq <- reads_nq[!(part & part_tgt %in% maps)]
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
