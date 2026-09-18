#' Shared mechanics of the model-layer database preparations
#'
#' The GTAP-AEZ, GTAP-E and GTAP-EP layers ([.prepare_aez()],
#' [.prepare_e()], [.prepare_ep()]) each synthesize the set headers
#' flexagg would build at aggregation and bind the parameter headers
#' the model reads under other names. The set construction, the bloc
#' header reclass, the CDE parameter promotion and the final append
#' are the same in every layer and live here; the layer files hold
#' only the element lists.
#'
#' @keywords internal
#' @noRd
NULL

#' Elements of the set header `h` (case-insensitive), lowercased
#'
#' @keywords internal
#' @noRd
.layer_elements <- function(i_data, h) {
  i <- match(h, toupper(names(i_data)))
  elements <- tolower(trimws(as.character(i_data[[i]])))
  return(elements)
}

#' Append the synthesized headers and mark the layer prepared. The
#' set / par / dat grouping is kept: new sets go after the leading
#' block of sets (the reclassed bloc headers sit among the parameters
#' and are not that block's end), new parameters after the last
#' parameter. `attrs` are the input list's attributes, taken before
#' any subsetting dropped them; the metadata flag `flag` marks the
#' layer prepared so the hooks do not run twice.
#'
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
