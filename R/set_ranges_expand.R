#' @keywords internal
#' @noRd
.expand_ele_range <- function(item,
                              bad_set,
                              call) {
  bad_ele <- item
  ends <- trimws(strsplit(item, "-", fixed = TRUE)[[1]])
  if (length(ends) != 2L || !all(nzchar(ends))) {
    range_reason <- model_err$set_ele_range_reason$form
    .cli_action(model_err$set_ele_range,
      action = c("abort", "inform"),
      call = call
    )
  }
  m <- regmatches(ends, regexec("^([A-Za-z_@][A-Za-z0-9_@]*?)([0-9]+)$", ends))
  if (any(lengths(m) %in% 0L) || !tolower(m[[1]][2]) %=% tolower(m[[2]][2])) {
    range_reason <- model_err$set_ele_range_reason$stem
    .cli_action(model_err$set_ele_range,
      action = c("abort", "inform"),
      call = call
    )
  }
  digits <- c(m[[1]][3], m[[2]][3])
  padded <- any(nchar(digits) > 1L & startsWith(digits, "0"))
  if (padded && !nchar(digits[1]) %=% nchar(digits[2])) {
    range_reason <- model_err$set_ele_range_reason$width
    .cli_action(model_err$set_ele_range,
      action = c("abort", "inform"),
      call = call
    )
  }
  lo <- as.numeric(digits[1])
  hi <- as.numeric(digits[2])
  if (hi < lo) {
    range_reason <- model_err$set_ele_range_reason$backwards
    .cli_action(model_err$set_ele_range,
      action = c("abort", "inform"),
      call = call
    )
  }
  width <- if (padded) {
    nchar(digits[1])
  } else {
    1L
  }
  expanded <- sprintf("%s%0*d", m[[1]][2], width, as.integer(seq(lo, hi)))
  return(expanded)
}

#' @keywords internal
#' @noRd
.expand_set_ranges <- function(sets,
                               call) {
  is_list <- !is.na(sets$definition) &
    grepl("^\\s*\\(", sets$definition) &
    !grepl("[", sets$definition, fixed = TRUE) &
    grepl("-", sets$definition, fixed = TRUE)
  for (i in which(is_list)) {
    inner <- sub("^\\s*\\(", "", sub("\\)\\s*$", "", trimws(sets$definition[i])))
    eles <- trimws(strsplit(inner, ",", fixed = TRUE)[[1]])
    ranged <- grepl("-", eles, fixed = TRUE)
    eles <- as.list(eles)
    eles[ranged] <- lapply(eles[ranged], .expand_ele_range, bad_set = sets$name[i], call = call)
    sets$definition[i] <- paste0("(", paste(unlist(eles), collapse = ", "), ")")
  }
  return(sets)
}
