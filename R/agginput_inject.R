#' @importFrom purrr pluck
#' @importFrom data.table CJ setnames fsetequal
#' 
#' @keywords internal
#' @noRd
.inject_agg_input <- function(.data,
                              sets,
                              model,
                              call) {

  agg_input <- attributes(model)[purrr::map_lgl(attributes(model), data.table::is.data.table)]

  for (nme in names(agg_input)) {
    input <- agg_input[[nme]]
    ls_upper <- purrr::pluck(model, "ls_upper_idx", nme)
    header <- model[which(model$name == nme & model$type == "Coefficient"),]$header
    set_ele <- with(sets$ele, mget(ls_upper))
    template_shk <- do.call(data.table::CJ, c(set_ele, sorted = FALSE))
    data.table::setnames(template_shk, new = colnames(input[, !"Value"]))
    data.table::setkey(template_shk)
    
    if (!data.table::fsetequal(input[, !"Value"], template_shk)) {
      tup <- .capture_dt_tup(
        template = template_shk,
        value = input[, !"Value"],
        drop = 3
      )
      .cli_action(
        deploy_err$agg_missing_tup,
        action = "abort",
        call = call
      )
    }
    data.table::setnames(input, new = c(ls_upper, "Value"))

    if (header %in% names(.data)) {
      classes <- class(purrr::pluck(.data, header))
      class(input) <- classes
      purrr::pluck(.data, header) <- input
    } else {
      classes <- setdiff(class(input), grep("_", class(input), value = TRUE))
      param <- grepl("parameter", model[(model$name == nme) & (model$type == "Coefficient"), ]$qualifier_list)
      type <- if (param) {"par"} else {"dat"}
      class(input) <- c(nme, type, classes)
      .data <- c(.data, list(input))
    }
  }

  return(.data)
}