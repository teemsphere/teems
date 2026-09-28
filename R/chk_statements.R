#' @importFrom purrr map_chr
#' @importFrom tools toTitleCase
#' @importFrom utils packageVersion
#' @keywords internal
#' @noRd
.check_statements <- function(tab,
                              call) {

  n_comments <- .strip_strong_comments(tab, call = call)
  n_comments <- paste(unlist(strsplit(n_comments, "![^!]*!", perl = TRUE)), collapse = "")
  n_comments <- .protect_label_semicolons(n_comments)
  statements <- unlist(strsplit(n_comments, ";", perl = TRUE))

  statements <- gsub("\r|\n", " ", statements, perl = TRUE)
  statements <- gsub("\\s{2,}", " ", statements)
  statements <- trimws(statements)
  statements <- statements[nzchar(statements)]

  state_kw <- c(supported_state, invalid_state)
  statements <- sub(
    paste0("^(", paste(state_kw, collapse = "|"), ")\\("),
    "\\1 (",
    statements,
    ignore.case = TRUE,
    perl = TRUE
  )

  fe <- grep("^\\s*formula\\s*(\\(\\s*initial\\s*\\))?\\s*&\\s*equation\\b",
    statements,
    ignore.case = TRUE
  )
  if (length(fe) > 0L) {
    statements <- as.list(statements)
    for (s in fe) {
      statements[[s]] <- .expand_formula_equation(statements[[s]], call = call)
    }
    statements <- unlist(statements)
  }

  statements <- .normalize_statements(statements, call = call)
  statements <- .continue_assertion_labels(statements)

  stray <- startsWith(statements, "#")
  if (any(stray)) {
    bad_stmt <- substr(statements[stray][1], 1L, 80L)
    .cli_action(model_err$stray_label,
      action = c("abort", "inform"),
      call = call
    )
  }

  state_decl <- tolower(unique(purrr::map_chr(strsplit(statements, " ", perl = TRUE), 1)))

  if (any(state_decl %in% tolower(invalid_state))) {
    inv_state <- state_decl[state_decl %in% tolower(invalid_state)]
    inv_state <- tools::toTitleCase(inv_state)
    version <- utils::packageVersion("teems")
    .cli_action(model_err$invalid_state,
                action = c("abort", "inform"),
                call = call)
  }
  
  if (any(!state_decl %in% tolower(supported_state))) {
    for (s in seq_len(length(statements))) {
      statement <- strsplit(statements[s], split = " ")[[1]][1]
      state_check <- paste0("^(", paste(supported_state, collapse = "|"), ")(?![A-Za-z0-9_@])")
      if (!grepl(state_check, statement, ignore.case = TRUE, perl = TRUE)) {
        implicit_stat <- strsplit(statements[s - 1], split = " ")[[1]][1]
        statements[s] <- paste(implicit_stat, statements[s])
      }
    }
  }

  state_decl <- tolower(unique(purrr::map_chr(strsplit(statements, " ", perl = TRUE), 1)))
  if (any(!state_decl %in% tolower(supported_state))) {
    state_decl <- unique(purrr::map_chr(strsplit(statements, " ", perl = TRUE), 1))
    unsupported <- state_decl[!tolower(state_decl) %in% tolower(supported_state)]
    unsupported <- tools::toTitleCase(unsupported)
    .cli_action(model_err$unsupported_tab,
      action = "abort",
      call = call
    )
  }

  statements <- .drop_set_writes(statements)
  statements <- .canonical_keywords(statements)
  statements <- .expand_tab_defaults(statements, call = call)
  return(statements)
}

#' @keywords internal
#' @noRd
.expand_formula_equation <- function(statement,
                                     call) {
  rem <- sub(
    "^\\s*formula\\s*(\\(\\s*initial\\s*\\))?\\s*&\\s*equation\\s*(\\(\\s*levels\\s*\\))?\\s*",
    "",
    statement,
    ignore.case = TRUE
  )
  m <- regmatches(
    rem,
    regexec("^([A-Za-z0-9_@]+)\\s*(#[^#]*#)?\\s*(.+)$", rem)
  )[[1]]
  if (length(m) == 0L || !grepl("=", m[4])) {
    bad_stmt <- statement
    .cli_action(model_err$formula_equation,
      action = "abort",
      call = call
    )
  }
  out <- c(
    paste("Formula (initial)", m[4]),
    trimws(paste("Equation (levels)", m[2], m[3], m[4]))
  )
  statements <- gsub("\\s{2,}", " ", out)
  return(statements)
}
