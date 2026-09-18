# conditional set builders `Set X = (all,i,SRC: <cond>);` (manual
# 10.1.2): the solver evaluates the data-dependent condition from the
# deployed input files ahead of set resolution
# (tab_setbuilder_transform); R mirrors it at deploy from the same
# aggregated tables (.eval_set_builder) so closure/shock validation and
# compose see the elements. Parsed here for shape only.
#' @keywords internal
#' @noRd
.parse_set_builders <- function(sets,
                                call) {
  is_builder <- !is.na(sets$definition) &
    grepl("^\\s*=\\s*\\(\\s*all\\s*,", sets$definition, ignore.case = TRUE)
  for (i in which(is_builder)) {
    bad_set <- sets$name[i]
    bad_def <- trimws(sets$definition[i])
    b <- .parse_set_builder(sets$definition[i])
    if (is.null(b)) {
      .cli_action(model_err$set_builder_cond,
        action = c("abort", "inform"),
        call = call
      )
    }
    if (tolower(sets$qualifier_list[i]) %=% "(intertemporal)") {
      .cli_action(model_err$set_builder_int,
        action = "abort",
        call = call
      )
    }
    if (tolower(b$src) %=% tolower(sets$name[i])) {
      .cli_action(model_err$set_self_ref,
        action = c("abort", "inform"),
        call = call
      )
    }
    src_idx <- match(tolower(b$src), tolower(sets$name))
    if (is.na(src_idx)) {
      bad_stmt <- paste("Set", sets$name[i], bad_def)
      bad_refs <- b$src
      .cli_action(model_err$set_undeclared,
        action = c("abort", "inform"),
        call = call
      )
    }
    # canonical spelling of the source set for the downstream exact
    # matches; the deployed statement is the author's text
    sets$definition[i] <- sprintf(
      "= (all,%s,%s: %s)", b$idx, sets$name[src_idx], b$cond
    )
  }

  if (any(grepl(":", sets$definition[!is_builder]))) {
    .cli_action(model_err$binary_switch,
                action = c("abort", "inform", "inform"),
                call = call)
  }
  parsed <- list(sets = sets, is_builder = is_builder)
  return(parsed)
}
