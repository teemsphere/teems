#' @importFrom data.table data.table setattr
#' @importFrom stats setNames
#' @keywords internal
#' @noRd
.eval_set_builder_formula <- function(spec,
                                      owner,
                                      src_map,
                                      mappings,
                                      coeff_data,
                                      model,
                                      set_raw,
                                      call) {
  limit_row <- which(model$type %in% "Set" & tolower(model$name) == tolower(owner))[1]
  ctx <- .sbx_ctx(model, coeff_data, mappings, set_raw, limit_row)
  src_ele <- tolower(unique(src_map$mapping))
  bad_set <- owner
  builder_cond <- spec$cond
  sel <- tryCatch(
    {
      cond <- .sbx_parse(spec$cond)
      binds <- stats::setNames(list(list(set = spec$src, ele = src_ele)), spec$idx)
      val <- .sbx_num(.sbx_eval(cond, binds, ctx))
      if (length(setdiff(val$idx, spec$idx)) > 0L) {
        .sbx_fail(.sbx_reason("free_index", paste(setdiff(val$idx, spec$idx), collapse = ", ")))
      }
      ele <- stats::setNames(list(src_ele), spec$idx)
      .sbx_expand(val, spec$idx, ele) != 0
    },
    sbx_defer = \(e) NULL,
    sbx_error = \(e) {
      reason <- conditionMessage(e)
      .cli_action(deploy_err$set_builder_eval,
        action = "abort",
        call = call
      )
    }
  )
  if (is.null(sel)) {
    return(NULL)
  }
  if (!any(sel)) {
    src_set <- spec$src
    if (.o_verbose()) {
      .cli_action(deploy_info$set_builder_empty,
        action = c("inform", "inform"),
        call = call
      )
    }
  }
  out <- src_map[tolower(src_map$mapping) %in% src_ele[sel], ]
  indicator <- data.table::data.table(src_ele, as.numeric(sel))
  names(indicator) <- c(spec$src, "Value")
  class(indicator) <- c(spec$header, "dat", class(indicator))
  data.table::setattr(out, "origin_conflict", NULL)
  data.table::setattr(out, "sb_indicator", indicator)
  return(out)
}
