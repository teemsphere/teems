#' @keywords internal
#' @noRd
.layer_elements <- function(i_data, h) {
  i <- match(h, toupper(names(i_data)))
  elements <- tolower(trimws(as.character(i_data[[i]])))
  return(elements)
}

#' @importFrom purrr map_lgl
#' @keywords internal
#' @noRd
.layer_finish <- function(i_data, new_sets, new_par = list(), attrs, flag) {
  out <- unclass(i_data)
  is_set <- purrr::map_lgl(out, inherits, "set")
  at_set <- if (isTRUE(is_set[[1]])) {
    rle(is_set)$lengths[[1]]
  } else {
    0L
  }
  out <- append(out, new_sets, after = at_set)
  if (length(new_par) > 0L) {
    at_par <- max(which(purrr::map_lgl(out, inherits, "par")), 0L)
    out <- append(out, new_par, after = at_par)
  }
  for (a in setdiff(names(attrs), c("names", "class", "metadata"))) {
    attr(out, a) <- attrs[[a]]
  }
  metadata <- attrs[["metadata"]]
  metadata[[flag]] <- TRUE
  attr(out, "metadata") <- metadata
  class(out) <- attrs[["class"]]
  return(out)
}
