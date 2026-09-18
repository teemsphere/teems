#' A 0/1 indicator coefficient for a data-comparison IF condition in an
#' Equation (the shipped MAKESUNIT/E_qca adaptation shape). Cached by
#' the condition with index names canonicalized to their sets, so the
#' same condition in another equation reuses one indicator. Returns
#' list(pre, ref) where ref uses the current statement's index names.
#'
#' @keywords internal
#' @noRd
.if_indicator <- function(cond_info,
                          quant,
                          q_idx,
                          synth,
                          if_cond,
                          call) {
  argstr <- sub("^[A-Za-z_][A-Za-z0-9_]*", "", cond_info$ref)
  args <- if (nchar(argstr) > 0L) {
    trimws(strsplit(gsub("[()]", "", argstr), ",")[[1]])
  } else {
    character(0)
  }
  is_idx <- !grepl('^"', args)
  at <- match(tolower(args[is_idx]), tolower(q_idx))
  if (length(args[is_idx]) %=% 0L || anyNA(at)) {
    # scalar conditions have no quantifier to carry the comparison
    .cli_action(model_err$invalid_if_cond,
      action = c("abort", "inform"),
      call = call
    )
  }
  dims <- args[is_idx]
  sets <- purrr::map_chr(quant[at], "set")
  canon <- args
  canon[is_idx] <- toupper(sets)
  key <- paste0(
    "CMP|", toupper(sub("\\(.*$", "", cond_info$ref)),
    "(", paste(canon, collapse = ","), ")|", cond_info$op, "|", cond_info$num
  )
  ind <- synth[[key]]
  pre <- character(0)
  if (is.null(ind)) {
    ind <- .synth_coeff_name(synth)
    synth[[key]] <- ind
    quants <- sprintf("(all,%s,%s)", dims, sets)
    n <- length(quants)
    quants_cond <- c(
      quants[-n],
      sprintf(
        "(all,%s,%s: %s %s %s)",
        dims[n], sets[n], cond_info$ref, cond_info$op, cond_info$num
      )
    )
    dimargs <- paste(dims, collapse = ",")
    pre <- c(
      sprintf(
        "Coefficient %s %s(%s) # if-rewrite indicator %s #",
        paste0(quants, collapse = ""), ind, dimargs, if_cond
      ),
      sprintf(
        "Formula %s %s(%s) = 0",
        paste0(quants, collapse = ""), ind, dimargs
      ),
      sprintf(
        "Formula %s %s(%s) = 1",
        paste0(quants_cond, collapse = ""), ind, dimargs
      )
    )
  }
  indicator <- list(pre = pre, ref = sprintf("%s(%s)", ind, paste(dims, collapse = ",")))
  return(indicator)
}
