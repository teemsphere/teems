#' @keywords internal
#' @noRd
.check_subset_containment <- function(sets,
                                      call) {
  for (i in seq_len(nrow(sets))) {
    subs <- sets$subsets[[i]]
    subs <- subs[!is.na(subs)]
    if (length(subs) == 0L) {
      next
    }
    super_ele <- sets$ele[[i]]
    for (nm in subs) {
      j <- match(nm, sets$name)
      if (is.na(j)) {
        next
      }
      missing_ele <- setdiff(sets$ele[[j]], super_ele)
      if (length(missing_ele) > 0L) {
        bad_sub <- sets$name[j]
        bad_super <- sets$name[i]
        .cli_action(model_err$subset_not_contained,
          action = c("abort", "inform"),
          call = call
        )
      }
    }
  }
  return(invisible(NULL))
}
