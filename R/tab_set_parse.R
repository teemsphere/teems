#' @keywords internal
#' @noRd
.parse_tab_sets <- function(extract,
                            call) {
  sets <- extract[tolower(extract$type) %in% "set", ]
  sets$type <- "Set"

  sets <- .parse_set_fields(sets, call = call)

  sets$remainder <- .advance_remainder(
    remainder = sets$remainder,
    pattern = sets$definition
  )

  if (any(sets$remainder != "")) {
    .cli_action(model_err$set_parse_fail,
      action = "abort",
      call = call,
      .internal = TRUE
    )
  }

  sets <- .expand_set_ranges(sets, call = call)

  fixed_int <- tolower(sets$qualifier_list) %in% "(intertemporal)" &
    !is.na(sets$definition) &
    grepl("^\\s*\\(", sets$definition) &
    !grepl("[", sets$definition, fixed = TRUE)
  sets$qualifier_list[fixed_int] <- "(non_intertemporal)"

  .chk_set_ele_lists(sets, call = call)

  builders <- .parse_set_builders(sets, call = call)
  sets <- builders$sets

  equalities <- .parse_set_equalities(
    sets = sets,
    is_builder = builders$is_builder,
    call = call
  )
  sets <- equalities$sets

  normalized <- .normalize_set_defs(
    sets = sets,
    is_builder = builders$is_builder,
    is_set_eq = equalities$is_set_eq
  )
  sets <- normalized$sets

  sets <- .chk_set_expr_refs(
    sets = sets,
    is_expr = normalized$is_expr,
    is_set_eq = equalities$is_set_eq,
    call = call
  )

  components <- .set_expr_components(
    sets = sets,
    is_expr = normalized$is_expr,
    is_builder = builders$is_builder
  )
  sets <- components$sets

  sets <- .parse_set_subsets(
    sets = sets,
    extract = extract,
    expr_info = components$expr_info,
    is_builder = builders$is_builder,
    is_set_eq = equalities$is_set_eq,
    call = call
  )

  sets <- .close_set_subsets(sets)

  sets$ls_upper_idx <- NA
  sets$ls_mixed_idx <- NA
  sets <- sets[, c("type",
                   "name",
                   "label",
                   "qualifier_list",
                   "ls_upper_idx",
                   "ls_mixed_idx",
                   "header",
                   "file",
                   "definition",
                   "subsets",
                   "comp1",
                   "comp2",
                   "row_id")]
  
  return(sets)
}
