#' @importFrom stats setNames
#' @keywords internal
#' @noRd
.eval_indicator <- function(steps, over, mappings, model = NULL) {
  vals <- stats::setNames(rep(0, length(over)), tolower(over))
  for (s in steps) {
    m <- mappings[[s$set]]
    if (is.null(m)) {
      ele <- .if_rewrite_elements(model, s$set)
      if (is.null(ele)) {
        return(NULL)
      }
    } else {
      ele <- tolower(unique(m$mapping))
    }
    ele <- ele[ele %in% names(vals)]
    if (s$mode == "set") {
      vals[ele] <- s$value
    } else {
      vals[ele] <- vals[ele] + s$value
    }
  }
  return(vals)
}
