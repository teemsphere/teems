#' @importFrom data.table setDT setkeyv
#' @importFrom rlang is_integerish
#' @keywords internal
#' @noRd
.aggregate_array <- function(arr,
                             sets,
                             ndigits) {
  dn <- lapply(dimnames(arr), tolower)
  nms <- names(dn)
  nms[duplicated(nms)] <- paste0(nms[duplicated(nms)], ".1")

  cd <- .arr_codes(dn = dn, sets = sets)
  out_sizes <- vapply(cd$ulevs, length, integer(1))
  val <- agg_array_sum(arr, cd$codes, out_sizes)

  cols <- .arr_expand_cols(
    ulevs = cd$ulevs,
    out_sizes = out_sizes,
    keep = rep(TRUE, length(cd$ulevs)),
    n_out = length(val)
  )
  names(cols) <- nms

  if (is.integer(arr)) {
    val <- as.integer(val)
  }
  dt <- data.table::setDT(c(cols, list(Value = val)))
  if (!rlang::is_integerish(dt$Value)) {
    dt[, let(Value = round(Value, ndigits))]
  }
  data.table::setkeyv(dt, nms)
  class(dt) <- c(class(arr)[1:2], class(dt))
  return(dt)
}