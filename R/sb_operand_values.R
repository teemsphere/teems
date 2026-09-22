#' @importFrom rlang is_integerish
#' @keywords internal
#' @noRd
.sb_operand_values <- function(coef, over, mappings, coeff_data, coeff_extract, model, bad_set, call) {
  ci <- match(tolower(coef), tolower(coeff_extract$name))
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
  if (!is.null(dt)) {
    cols <- setdiff(names(dt), "Value")
    if (length(cols) != 1L) {
      cond_coef <- coef
      n_args <- 1L
      n_dims <- length(cols)
      .cli_action(deploy_err$set_builder_args,
        action = "abort",
        call = call
      )
    }
    val <- dt$Value
    is_int <- !is.na(coeff_extract$qualifier_list[ci]) &&
      grepl("integer", coeff_extract$qualifier_list[ci], ignore.case = TRUE)
    val <- if (is_int || rlang::is_integerish(val)) {
      as.integer(val)
    } else {
      round(val, .o_ndigits())
    }
    e <- tolower(dt[[cols]])
    out <- vapply(tolower(over), \(x) {
      hit <- which(e == x)
      if (length(hit) == 0L) {
        0
      } else {
        val[hit[1]]
      }
    }, numeric(1))
    return(out)
  }
  steps <- if (is.null(model)) {
    NULL
  } else {
    .indicator_formulas(model, coef)
  }
  if (is.null(steps)) {
    cond_coef <- coef
    .cli_action(deploy_err$set_builder_data,
      action = "abort",
      call = call
    )
  }
  values <- .eval_indicator(steps, over, mappings, model = model)
  return(values)
}
