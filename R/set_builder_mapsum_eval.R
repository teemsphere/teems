#' The mapping-sum builder `(all,i,SRC: sum{j,DOM: MAP(j) = i,
#' COEF(j)} <op> c)`: SRC must be the mapping's codomain and DOM its
#' domain (the solver's contract); the mapping values are composed
#' under the active aggregation exactly as the deployed by_elements
#' header is (.compose_map_values), the operand is file-Read or an
#' indicator. NULL while the domain, codomain or an operand set is
#' unresolved.
#'
#' @keywords internal
#' @noRd
.eval_set_builder_mapsum <- function(b,
                                     owner,
                                     src_map,
                                     mappings,
                                     coeff_data,
                                     coeff_extract,
                                     model,
                                     set_raw,
                                     call) {
  bad_set <- owner
  cond_map <- b$map
  map_row <- if (is.null(model)) {
    NULL
  } else {
    model[model$type == "Mapping" & tolower(model$name) == tolower(b$map), ]
  }
  if (is.null(map_row) || nrow(map_row) == 0L) {
    .cli_action(deploy_err$set_builder_mapsum,
      action = c("abort", "inform"),
      call = call
    )
  }
  dom <- map_row$comp1[1]
  cod <- map_row$comp2[1]
  if (tolower(dom) != tolower(b$sum_set) || tolower(cod) != tolower(b$src)) {
    .cli_action(deploy_err$set_builder_mapsum,
      action = c("abort", "inform"),
      call = call
    )
  }
  set_names <- names(mappings)
  dom_map <- mappings[[set_names[match(tolower(dom), tolower(set_names))]]]
  if (is.null(dom_map)) {
    return(NULL)
  }
  byele <- model$type == "Read" & !is.na(model$qualifier_list) &
    grepl("by_elements", model$qualifier_list, ignore.case = TRUE) &
    tolower(model$name) == tolower(b$map)
  header <- model$header[byele][1]
  raw_idx <- if (is.na(header)) {
    NA_integer_
  } else {
    match(toupper(header), toupper(names(set_raw)))
  }
  if (is.na(raw_idx)) {
    .cli_action(deploy_err$set_builder_mapsum,
      action = c("abort", "inform"),
      call = call
    )
  }
  vals <- set_raw[[raw_idx]]
  dom_header <- NA_character_
  if (!is.null(model)) {
    di <- which(model$type == "Set" & tolower(model$name) == tolower(dom))
    if (length(di) > 0L) {
      dom_header <- model$header[di[1]]
    }
  }
  dom_raw_idx <- if (is.na(dom_header)) {
    NA_integer_
  } else {
    match(toupper(dom_header), toupper(names(set_raw)))
  }
  dom_orig <- if (!is.na(dom_raw_idx)) {
    set_raw[[dom_raw_idx]]
  } else {
    unique(dom_map$origin)
  }
  agg_ele <- unique(dom_map$mapping)
  map_name <- b$map
  composed <- .compose_map_values(
    vals = vals, dom_orig = dom_orig, dom_map = dom_map, cod_map = src_map,
    agg_ele = agg_ele, map_name = map_name, dom = dom, cod = cod,
    header = header, call = call
  )

  vals_coef <- .sb_operand_values(
    coef = b$coef, over = agg_ele, mappings = mappings,
    coeff_data = coeff_data, coeff_extract = coeff_extract,
    model = model, bad_set = owner, call = call
  )
  if (is.null(vals_coef)) {
    return(NULL)
  }
  src_ele <- unique(src_map$mapping)
  sel <- vapply(src_ele, \(x) {
    acc <- sum(vals_coef[tolower(composed) == tolower(x)])
    isTRUE(.sb_op_test(acc, b$op, b$const))
  }, logical(1))
  if (!any(sel)) {
    src_set <- b$src
    builder_cond <- b$cond
    .cli_action(deploy_err$set_builder_empty,
      action = "abort",
      call = call
    )
  }
  out <- src_map[src_map$mapping %in% src_ele[sel], ]
  data.table::setattr(out, "origin_conflict", NULL)
  return(out)
}
