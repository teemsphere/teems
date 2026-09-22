#' @keywords internal
#' @noRd
.chk_tab_stmt_length <- function(statements,
                                 call) {
  max_len <- 20000L
  compact <- gsub("\\s+", " ", gsub("#[^#]*#", "", statements))
  n_char <- nchar(compact) + 20L
  over <- n_char > max_len
  kw <- tolower(sub("^\\s*([A-Za-z_]+).*$", "\\1", compact))
  grow <- kw %in% c("equation", "update") & !over & n_char > max_len %/% 2L
  if (any(grow)) {
    var_decl <- compact[kw == "variable"]
    var_decl <- sub("^\\s*variable\\s*", "", var_decl, ignore.case = TRUE)
    while (any(grepl("\\([^()]*\\)", var_decl))) {
      var_decl <- gsub("\\([^()]*\\)", " ", var_decl)
    }
    var_names <- unique(tolower(trimws(sub("[;[:space:]].*$", "", trimws(var_decl)))))
    var_names <- var_names[nzchar(var_names)]
    for (i in which(grow)) {
      txt <- tolower(gsub("\"[^\"]*\"", "\"\"", compact[i]))
      tokens <- regmatches(txt, gregexpr("[a-z_][a-z0-9_@]*", txt))[[1]]
      n_char[i] <- n_char[i] + 2L * sum(tokens %in% var_names)
    }
    over <- n_char > max_len
  }
  if (any(over)) {
    bad_stmt <- paste0(substr(trimws(compact[over][1]), 1, 60), "...")
    stmt_len <- n_char[over][1]
    .cli_action(model_err$stmt_too_long,
      action = c("abort", "inform"),
      call = call
    )
  }
  return(invisible(NULL))
}
