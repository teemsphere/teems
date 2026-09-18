#' Character-level bracket depth before each character; '(' '[' '{' are
#' interchangeable (manual 11.4.6). Quoted element literals are opaque.
#'
#' @keywords internal
#' @noRd
.tab_scan <- function(s) {
  chs <- strsplit(s, "")[[1]]
  qcum <- cumsum(chs == '"')
  in_quote <- qcum %% 2L == 1L | chs == '"'
  opens <- chs %in% c("(", "[", "{") & !in_quote
  closes <- chs %in% c(")", "]", "}") & !in_quote
  depth <- cumsum(opens) - cumsum(closes)
  scan <- list(
    chs = chs,
    in_quote = in_quote,
    depth_before = c(0L, depth[-length(chs)])
  )
  return(scan)
}
