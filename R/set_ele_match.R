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
      # an empty set (manual 11.7.9; an IF rewrite's complement partition
      # can be empty) has no rows: seq(start, stop) would count backwards
      if (l < 1) {
        return(character(0))
      }
      setele_dt[row_id + seq_len(l), "mapped_ele"][[1]]
    }
  )

  names(sets_out$ele) <- sets_out$setname
  return(sets_out)
}