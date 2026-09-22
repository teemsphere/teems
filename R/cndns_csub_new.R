#' @importFrom stats setNames
#' @keywords internal
#' @noRd
.csub_new <- function(expr,
                      binding,
                      label,
                      csub) {
  dims <- names(binding)
  canon_map <- stats::setNames(paste0("d", seq_along(dims)), dims)
  key <- paste0(
    .rename_expr_tokens(expr, canon_map),
    "|",
    paste(unname(binding), collapse = ",")
  )

  cached <- csub$cache[[key]]
  if (!is.null(cached)) {
    csub <- .csub_ref(cached, dims)
    return(csub)
  }

  repeat {
    csub$counter <- csub$counter + 1L
    name <- paste0("CSUB", csub$counter)
    if (!tolower(name) %in% csub$taken) {
      break
    }
  }

  quant_text <- paste0(
    "(all,", dims, ",", unname(binding), ")",
    collapse = ""
  )
  if (length(dims) == 0L) {
    quant_text <- ""
  }

  csub$statements <- c(
    csub$statements,
    paste0(
      "Coefficient ", quant_text, " ", .csub_ref(name, dims),
      " # ", label, " #"
    ),
    paste0(
      "Formula ", quant_text, " ", .csub_ref(name, dims),
      " = ", expr
    )
  )
  csub$cache[[key]] <- name
  csub <- .csub_ref(name, dims)
  return(csub)
}
