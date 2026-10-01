#' @keywords internal
#' @noRd
.cond_unwrap <- function(cond) {
  cond <- trimws(cond)
  while (grepl("^[[({]", cond) && isTRUE(.match_bracket(cond, 1L) == nchar(cond))) {
    inner <- trimws(substr(cond, 2L, nchar(cond) - 1L))
    scan <- .tab_scan(inner)
    top <- scan$depth_before == 0L & !scan$in_quote
    if (!any(top & scan$chs %in% c("<", ">", "=")) && length(.cond_logic_ops(inner)) %=% 0L) {
      break
    }
    cond <- inner
  }
  return(cond)
}
