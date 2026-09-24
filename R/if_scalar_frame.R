#' @keywords internal
#' @noRd
.synth_scalar_frame <- function(synth) {
  if (!is.null(synth$frame)) {
    frame <- list(pre = character(0), set = synth$frame, idx = synth$frame_idx)
    return(frame)
  }
  nm <- .synth_unique_name(synth, "IFO")
  synth$frame <- nm
  synth$frame_idx <- paste0(tolower(nm), "i")
  frame <- list(
    pre = sprintf("Set %s # if-rewrite scalar frame # (%s)", nm, paste0(tolower(nm), "e")),
    set = nm,
    idx = synth$frame_idx
  )
  return(frame)
}

#' @keywords internal
#' @noRd
.synth_unique_name <- function(synth, prefix) {
  ctr <- paste0("n_", prefix)
  if (is.null(synth[[ctr]])) {
    synth[[ctr]] <- 0L
  }
  repeat {
    synth[[ctr]] <- synth[[ctr]] + 1L
    nm <- paste0(prefix, synth[[ctr]])
    hit <- paste0("(^|[^A-Za-z0-9_@])", nm, "([^A-Za-z0-9_@]|$)")
    if (!any(grepl(hit, synth$tab, ignore.case = TRUE))) {
      return(nm)
    }
  }
}

#' @keywords internal
#' @noRd
.rewrite_scalar_if <- function(label,
                               qual_groups,
                               lhs,
                               rhs,
                               synth,
                               call) {
  fr <- .synth_scalar_frame(synth)
  h <- .synth_unique_name(synth, "IFV")
  qual <- if (length(qual_groups) > 0L) {
    paste0(paste(qual_groups, collapse = ""), " ")
  } else {
    ""
  }
  quant <- sprintf("(all,%s,%s)", fr$idx, fr$set)
  href <- sprintf("%s(%s)", h, fr$idx)
  inner <- .rewrite_formula_if(
    sprintf("Formula %s%s%s %s = %s", label, qual, quant, href, rhs),
    synth,
    call
  )
  statements <- c(
    fr$pre,
    sprintf("Coefficient %s %s # if-rewrite scalar frame for %s #", quant, href, lhs),
    inner,
    sprintf("Formula %s%s = sum(%s,%s,%s)", qual, lhs, fr$idx, fr$set, href)
  )
  statements <- gsub("\\s{2,}", " ", statements)
  return(statements)
}
