#' @importFrom purrr map_chr
#' @keywords internal
#' @noRd
.if_expr_helper <- function(cond_info,
                            quant,
                            q_idx,
                            qual_groups,
                            synth,
                            if_cond,
                            stmt,
                            call) {
  expr <- if (grepl("^[-+]?([0-9]+\\.?[0-9]*|\\.[0-9]+)([eE][-+]?[0-9]+)?$", cond_info$rhs)) {
    num <- cond_info$rhs
    cond_info$lhs
  } else {
    num <- "0"
    paste0("[", cond_info$lhs, "] - [", cond_info$rhs, "]")
  }
  var_names <- .tab_linear_variable_names(synth$tab)
  toks <- toupper(unique(regmatches(expr, gregexpr("[A-Za-z_][A-Za-z0-9_@]*", expr))[[1]]))
  toks <- setdiff(toks, toupper(q_idx[!is.na(q_idx)]))
  if (any(toks %in% var_names)) {
    if_statement <- stmt
    bad_vars <- toks[toks %in% var_names]
    .cli_action(model_err$if_cond_variable,
      action = c("abort", "inform"),
      call = call
    )
  }
  live <- !is.na(q_idx)
  used <- vapply(q_idx[live], \(ix) {
    grepl(paste0("(^|[^A-Za-z0-9_@])", ix, "([^A-Za-z0-9_@]|$)"), expr, ignore.case = TRUE)
  }, logical(1))
  dims <- q_idx[live][used]
  sets <- purrr::map_chr(quant[live][used], "set")
  canon <- toupper(gsub("\\s", "", expr))
  for (k in seq_along(dims)) {
    canon <- gsub(
      paste0("(^|[^A-Za-z0-9_@])", dims[k], "([^A-Za-z0-9_@]|$)"),
      paste0("\\1<", k, ":", sets[k], ">\\2"),
      canon,
      ignore.case = TRUE
    )
  }
  key <- paste0(
    "EXPR|", canon, "|", paste(toupper(sets), collapse = ","), "|",
    paste(qual_groups, collapse = "")
  )
  nm <- synth[[key]]
  pre <- character(0)
  if (is.null(nm)) {
    nm <- .synth_expr_name(synth)
    synth[[key]] <- nm
    quants <- paste0(sprintf("(all,%s,%s)", dims, sets), collapse = "")
    dimargs <- if (length(dims) > 0L) {
      paste0("(", paste(dims, collapse = ","), ")")
    } else {
      ""
    }
    qual <- if (length(qual_groups) > 0L) {
      paste0(paste(qual_groups, collapse = ""), " ")
    } else {
      ""
    }
    pre <- c(
      sprintf(
        "Coefficient %s%s%s # if-rewrite condition %s #",
        quants, if (nzchar(quants)) {
          " "
        } else {
          ""
        }, paste0(nm, dimargs), if_cond
      ),
      sprintf("Formula %s%s%s%s = %s", qual, quants, if (nzchar(quants)) {
        " "
      } else {
        ""
      }, paste0(nm, dimargs), expr)
    )
    pre <- gsub("\\s{2,}", " ", pre)
  }
  ref <- if (length(dims) > 0L) {
    paste0(nm, "(", paste(dims, collapse = ","), ")")
  } else {
    nm
  }
  helper <- list(
    pre = pre,
    cond_info = list(kind = "cmp", ref = ref, op = cond_info$op, num = num)
  )
  return(helper)
}
