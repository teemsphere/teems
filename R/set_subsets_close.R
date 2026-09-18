# transitive closure over direct subset relations; visited-set based
# because set equality creates mutual (cyclic) subset pairs
#' @keywords internal
#' @noRd
.close_set_subsets <- function(sets) {
  direct_subs <- sets$subsets
  for (i in seq_len(nrow(sets))) {
    nm <- sets$name[i]
    closure <- character(0)
    frontier <- direct_subs[[nm]]
    frontier <- frontier[!is.na(frontier)]
    while (length(frontier) > 0) {
      closure <- c(closure, frontier)
      nxt <- unlist(
        direct_subs[intersect(frontier, names(direct_subs))],
        use.names = FALSE
      )
      frontier <- setdiff(nxt[!is.na(nxt)], c(closure, nm))
    }
    if (length(closure) > 0) {
      sets$subsets[i] <- list(unique(closure))
    }
  }
  return(sets)
}
