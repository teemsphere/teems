#' @importFrom data.table setattr
#' @importFrom rlang is_integerish
#' @keywords internal
#' @noRd
.eval_set_builder <- function(b,
                              owner,
                              mappings,
                              coeff_data,
                              coeff_extract,
                              call,
                              model = NULL,
                              set_raw = NULL) {
  src_map <- mappings[[b$src]]
  if (is.null(src_map)) {
    return(NULL)
  }
  src_ele <- unique(src_map$mapping)
  bad_set <- owner

  if (b$form == "mapsum") {
    elements <- .eval_set_builder_mapsum(
      b = b, owner = owner, src_map = src_map, mappings = mappings,
      coeff_data = coeff_data, coeff_extract = coeff_extract,
      model = model, set_raw = set_raw, call = call
    )
    return(elements)
  }

  ci <- match(tolower(b$coef), tolower(coeff_extract$name))
  hdr <- if (is.na(ci)) {
    NA_character_
  } else {
    coeff_extract$header[ci]
  }
  dt <- if (is.na(hdr)) {
    NULL
  } else {
    coeff_data[[hdr]]
  }
  cond_coef <- b$coef
  spec <- if (is.null(model)) {
    NULL
  } else {
    attr(model, "set_builders")[[b$coef]]
  }
  if (is.null(dt) && !is.null(spec)) {
    elements <- .eval_set_builder_formula(
      spec = spec, owner = owner, src_map = src_map, mappings = mappings,
      coeff_data = coeff_data, model = model, set_raw = set_raw, call = call
    )
    return(elements)
  }
  if (is.null(dt) && length(b$args) == 1L) {
    steps <- if (is.null(model)) {
      NULL
    } else {
      .indicator_formulas(model, b$coef)
    }
    if (!is.null(steps)) {
      vals <- .eval_indicator(steps, src_ele, mappings, model = model)
      if (is.null(vals)) {
        return(NULL)
      }
      sel <- vapply(vals, \(vv) isTRUE(.sb_op_test(vv, b$op, b$const)), logical(1))
      if (!any(sel)) {
        src_set <- b$src
        builder_cond <- b$cond
        if (.o_verbose()) {
          .cli_action(deploy_info$set_builder_empty,
            action = c("inform", "inform"),
            call = call
          )
        }
      }
      out <- src_map[tolower(src_map$mapping) %in% names(vals)[sel], ]
      data.table::setattr(out, "origin_conflict", NULL)
      return(out)
    }
  }
  if (is.null(dt)) {
    .cli_action(deploy_err$set_builder_data,
      action = "abort",
      call = call
    )
  }
  cols <- setdiff(names(dt), "Value")
  if (length(cols) != length(b$args)) {
    n_args <- length(b$args)
    n_dims <- length(cols)
    .cli_action(deploy_err$set_builder_args,
      action = "abort",
      call = call
    )
  }

  val <- dt$Value
  is_int <- !is.na(coeff_extract$qualifier_list[ci]) &&
    grepl("integer", coeff_extract$qualifier_list[ci], ignore.case = TRUE)
  if (is_int || rlang::is_integerish(val)) {
    val <- as.integer(val)
  } else {
    val <- .round_digits(val, .o_ndigits())
  }

  keep_rows <- rep(TRUE, nrow(dt))
  for (d in seq_along(cols)) {
    if (d == b$loop_dim) {
      next
    }
    ele <- tolower(gsub('"', "", b$args[d]))
    col_vals <- tolower(dt[[cols[d]]])
    if (!ele %in% col_vals) {
      bad_ele <- ele
      dim_set <- cols[d]
      .cli_action(deploy_err$set_builder_ele,
        action = "abort",
        call = call
      )
    }
    keep_rows <- keep_rows & col_vals == ele
  }
  loop_vals <- tolower(dt[[cols[b$loop_dim]]])
  loop_ele <- unique(loop_vals)
  if (!setequal(loop_ele, tolower(src_ele))) {
    loop_idx <- b$idx
    dim_set <- cols[b$loop_dim]
    src_set <- b$src
    .cli_action(deploy_err$set_builder_dim,
      action = "abort",
      call = call
    )
  }

  v <- val[keep_rows]
  e <- loop_vals[keep_rows]
  sel <- vapply(tolower(src_ele), \(x) {
    hit <- which(e == x)
    vv <- if (length(hit) == 0L) {
      0
    } else {
      v[hit[1]]
    }
    isTRUE(.sb_op_test(vv, b$op, b$const))
  }, logical(1))

  if (!any(sel)) {
    src_set <- b$src
    builder_cond <- b$cond
    if (.o_verbose()) {
      .cli_action(deploy_info$set_builder_empty,
        action = c("inform", "inform"),
        call = call
      )
    }
  }
  kept <- src_ele[sel]
  out <- src_map[src_map$mapping %in% kept, ]
  data.table::setattr(out, "origin_conflict", NULL)
  return(out)
}
