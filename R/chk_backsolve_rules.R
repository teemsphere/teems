# GEMPACK manual 14.1.10: the eight requirements for a substitution
# (or backsolve) to be possible. Returns the common argument pattern.
#' @keywords internal
#' @noRd
.check_backsolve_rules <- function(var_name,
                                   entry,
                                   decl_sets,
                                   call) {
  eq_name <- entry$name
  all_terms <- c(entry$lhs, entry$rhs)
  occ_at <- which(purrr::map_lgl(all_terms, \(t) {
    !is.null(t$var) && t$var$name %=% var_name
  }))

  if (length(occ_at) == 0L) {
    rule_text <- paste0(
      "The variable does not occur in the equation."
    )
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

  fail <- function(rule_text) {
    msg <- .cli_action(model_err$condense_rule,
      action = c("abort", "inform", "inform"),
      call = call
    )
    return(msg)
  }

  for (o in occ_at) {
    t <- all_terms[[o]]
    args <- t$var$args

    if (any(grepl("^\"", args))) {
      fail(paste0(
        "Occurrence ", .serialize_term(t), ": an element occurs as an ",
        "argument; every argument must be an index (requirement 1)."
      ))
    }

    if (any(grepl("[+-]", args))) {
      fail(paste0(
        "Occurrence ", .serialize_term(t), ": an argument carries a ",
        "lead/lag offset; offsets block substitution in intertemporal ",
        "models (requirement 6)."
      ))
    }

    sum_idx <- purrr::map_chr(t$quants, "idx")
    if (any(args %in% sum_idx)) {
      fail(paste0(
        "Occurrence ", .serialize_term(t), ": a SUM index occurs as an ",
        "argument; every index must be an equation ALL index ",
        "(requirement 2)."
      ))
    }

    if (!all(quant_idx %in% args)) {
      missing_idx <- setdiff(quant_idx, args)
      fail(paste0(
        "Equation ALL index (", paste(missing_idx, collapse = ","),
        ") absent from occurrence ", .serialize_term(t),
        "; every equation ALL index must appear in each occurrence ",
        "(requirement 3)."
      ))
    }

    if (anyDuplicated(args)) {
      fail(paste0(
        "Occurrence ", .serialize_term(t), ": a repeated index; all ",
        "indices of one occurrence must be different (requirement 5)."
      ))
    }

    if (!all(args %in% quant_idx)) {
      fail(paste0(
        "Occurrence ", .serialize_term(t), ": an argument is not bound ",
        "by an equation ALL quantifier (requirement 2)."
      ))
    }

    arg_sets <- quant_set[match(args, quant_idx)]
    if (length(arg_sets) != length(decl_sets) ||
      !all(tolower(arg_sets) == tolower(decl_sets))) {
      fail(paste0(
        "Occurrence ", .serialize_term(t), " ranges over {",
        paste(arg_sets, collapse = ","), "} but the variable is declared ",
        "over {", paste(decl_sets, collapse = ","), "}; every index must ",
        "range over the full declared set (requirement 4)."
      ))
    }
  }

  first_args <- all_terms[[occ_at[[1]]]]$var$args
  for (o in occ_at[-1]) {
    if (!identical(all_terms[[o]]$var$args, first_args)) {
      fail(paste0(
        "Occurrences ", .serialize_term(all_terms[[occ_at[[1]]]]), " and ",
        .serialize_term(all_terms[[o]]), " have different index patterns; ",
        "all occurrences must share one pattern (requirement 7)."
      ))
    }
  }

  parsed <- list(args = first_args)
  return(parsed)
}
