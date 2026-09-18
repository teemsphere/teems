# equation k keeps membership term k as [value] and drops the others;
# the remainder equation (keep = 0L) drops every one of them
#' @keywords internal
#' @noRd
.split_eq_chunks <- function(keep,
                             side_terms,
                             membership,
                             if_pattern) {
  n_m <- length(membership)
    chunks <- side_terms
    drop <- list(integer(0), integer(0))
    for (j in seq_len(n_m)) {
      m <- membership[[j]]
      if (identical(j, keep)) {
        # a value carrying IF terms of its own is spread over the side
        # so the recursive pass sees them at the top level
        chunks[[m$side]][m$term] <- if (grepl(if_pattern, m$value)) {
          .distribute_terms(m$sign, m$value)
        } else {
          paste(m$sign, paste0("[", m$value, "]"))
        }
      } else {
        drop[[m$side]] <- c(drop[[m$side]], m$term)
      }
    }
    for (h in 1:2) {
      if (length(drop[[h]]) > 0L) {
        chunks[[h]] <- chunks[[h]][-drop[[h]]]
      }
    }
    return(chunks)
  return(chunks)
}
