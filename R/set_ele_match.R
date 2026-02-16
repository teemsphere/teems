#' @importFrom purrr map2
#' 
#' @keywords internal
#' @noRd
.match_set_ele <- function(sets_out,
                           setele_dt) {
  sets_out$ele <- purrr::map2(
    .x = sets_out[["begadd"]],
    .y = sets_out[["size"]],
    .f = function(row_id, l) {
      start <- row_id + 1
      stop <- row_id + l
      setele_dt[seq(start, stop), "mapped_ele"][[1]]
    }
  )

  names(sets_out$ele) <- sets_out$setname
  return(sets_out)
}