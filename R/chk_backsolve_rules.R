#' @keywords internal
#' @noRd
.abort_backsolve_rule <- function(rule_text, eq_name, var_name, call) {
  msg <- .cli_action(model_err$condense_rule,
    action = c("abort", "inform", "inform"),
    call = call
  )
  return(msg)
}

#' @importFrom purrr map_chr map_lgl
#' @keywords internal
#' @noRd
.check_backsolve_rules <- function(var_name,
                                   entry,
                                   decl_sets,
                                   call) {
  txt <- model_err$condense_rule_text
  eq_name <- entry$name
  all_terms <- c(entry$lhs, entry$rhs)
  occ_at <- which(purrr::map_lgl(all_terms, \(t) {
    !is.null(t$var) && t$var$name %=% var_name
  }))

  if (length(occ_at) == 0L) {
    rule_text <- txt$absent
    .cli_action(model_err$condense_rule,
      action = c("abort", "inform", "inform"),
      call = call
    )
  }

  quant_idx <- purrr::map_chr(entry$quants, "idx")
  quant_set <- purrr::map_chr(entry$quants, "set")
  if (length(decl_sets) == 1L && is.na(decl_sets)) {
    decl_sets <- character()
  }

  for (o in occ_at) {
    t <- all_terms[[o]]
    args <- t$var$args
    occ <- .serialize_term(t)

    if (any(grepl("^\"", args))) {
      .abort_backsolve_rule(
        sprintf(txt$element_arg, occ), eq_name, var_name, call
      )
    }

    if (any(grepl("[+-]", args))) {
      .abort_backsolve_rule(
        sprintf(txt$offset_arg, occ), eq_name, var_name, call
      )
    }

    sum_idx <- purrr::map_chr(t$quants, "idx")
    if (any(args %in% sum_idx)) {
      .abort_backsolve_rule(
        sprintf(txt$sum_index, occ), eq_name, var_name, call
      )
    }

    if (!all(quant_idx %in% args)) {
      missing_idx <- setdiff(quant_idx, args)
      .abort_backsolve_rule(
        sprintf(txt$missing_index, paste(missing_idx, collapse = ","), occ),
        eq_name, var_name, call
      )
    }

    if (anyDuplicated(args)) {
      .abort_backsolve_rule(
        sprintf(txt$repeated_index, occ), eq_name, var_name, call
      )
    }

    if (!all(args %in% quant_idx)) {
      .abort_backsolve_rule(
        sprintf(txt$unbound_arg, occ), eq_name, var_name, call
      )
    }

    arg_sets <- quant_set[match(args, quant_idx)]
    if (length(arg_sets) != length(decl_sets) ||
      !all(tolower(arg_sets) == tolower(decl_sets))) {
      .abort_backsolve_rule(
        sprintf(
          txt$partial_range, occ,
          paste(arg_sets, collapse = ","),
          paste(decl_sets, collapse = ",")
        ),
        eq_name, var_name, call
      )
    }
  }

  first_args <- all_terms[[occ_at[[1]]]]$var$args
  for (o in occ_at[-1]) {
    if (!identical(all_terms[[o]]$var$args, first_args)) {
      .abort_backsolve_rule(
        sprintf(
          txt$mixed_patterns,
          .serialize_term(all_terms[[occ_at[[1]]]]),
          .serialize_term(all_terms[[o]])
        ),
        eq_name, var_name, call
      )
    }
  }

  parsed <- list(args = first_args)
  return(parsed)
}
