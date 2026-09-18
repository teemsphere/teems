#' @keywords internal
#' @noRd
.chk_tab_defaults <- function(statements,
                              call) {
  # "# label #" text is free-form and may contain "(default ...)"
  # (gtapv7-mrio): scan the label-stripped statement only
  no_label <- gsub("#[^#]*#", "", statements)
  idx <- grep("\\(\\s*default", no_label, ignore.case = TRUE)
  for (i in idx) {
    stmt <- no_label[i]
    bad_stmt <- trimws(statements[i])
    kw <- tolower(sub("^\\s*([A-Za-z_]+).*$", "\\1", stmt))
    bad_val <- sub(".*?\\(\\s*default\\s*=?\\s*([^);]*).*$", "\\1", stmt, ignore.case = TRUE)
    bad_val <- tolower(gsub("[[:space:]]", "", bad_val))
    if (!kw %in% names(tab_default_values)) {
      .cli_action(model_err$default_keyword,
        action = "abort",
        call = call
      )
    }
    if (bad_val %in% tab_default_values[[kw]]) {
      # valid for the solver, but the teems pipeline cannot carry the
      # positional Default semantics through deploy yet
      .cli_action(model_err$default_unsupported,
        action = c("abort", "inform"),
        call = call
      )
    }
    if (kw == "coefficient" && grepl("^(lower|upper)_bound", bad_val)) {
      .cli_action(model_err$default_bound,
        action = "abort",
        call = call
      )
    }
    if (kw == "equation" && bad_val == "levels") {
      .cli_action(model_err$default_levels,
        action = "abort",
        call = call
      )
    }
    if (kw == "equation" && grepl("^add_homotopy", bad_val)) {
      .cli_action(model_err$default_homotopy,
        action = "abort",
        call = call
      )
    }
    default_kw <- tools::toTitleCase(kw)
    .cli_action(model_err$default_unknown,
      action = "abort",
      call = call
    )
  }
  return(invisible(NULL))
}
