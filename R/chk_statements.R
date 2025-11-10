#' @importFrom purrr map_chr
#' 
#' @keywords internal
#' @note Comprehensive statement-specific checks to be implemented here
#' @noRd
.check_statements <- function(tab,
                              ok_state,
                              call) {
  n_comments <- paste(unlist(strsplit(tab, "![^!]*!", perl = TRUE)), collapse = "")
  statements <- unlist(strsplit(n_comments, ";", perl = TRUE))

  statements <- gsub("\r|\n", " ", statements, perl = TRUE)
  statements <- gsub("\\s{2,}", " ", statements)
  statements <- trimws(statements[statements != " "])

  state_decl <- tolower(unique(purrr::map_chr(strsplit(statements, " ", perl = TRUE), 1)))

  if (any(!state_decl %in% tolower(ok_state))) {
    # make implicit decl explicit
    for (s in seq_len(length(statements))) {
      statement <- strsplit(statements[s], split = " ")[[1]][1]
      state_check <- paste0("\\b", ok_state, "\\b")
      if (!grepl(paste(state_check, collapse = "|"),
        statement,
        ignore.case = TRUE
      )) {
        implicit_stat <- strsplit(statements[s - 1], split = " ")[[1]][1]
        statements[s] <- paste(implicit_stat, statements[s])
      }
    }
  }

  # second check
  state_decl <- tolower(unique(purrr::map_chr(strsplit(statements, " ", perl = TRUE), 1)))
  if (any(!state_decl %in% tolower(ok_state))) {
    state_decl <- unique(purrr::map_chr(strsplit(statements, " ", perl = TRUE), 1))
    unsupported <- state_decl[!tolower(state_decl) %in% tolower(ok_state)]
    .cli_action(model_err$unsupported_tab,
      action = "abort",
      call = call
    )
  }

  return(statements)
}
