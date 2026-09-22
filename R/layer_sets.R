#' @importFrom purrr map
#' @keywords internal
#' @noRd
.layer_sets <- function(spec, elements, fmt) {
  absent <- setdiff(unique(spec$family), names(elements))
  if (length(absent) > 0L) {
    stop("layer_spec ", spec$flag, ": no elements for ", absent[[1]])
  }
  sets <- purrr::map(spec$headers, \(h) {
    .layer_set(h, elements[[spec$family[[h]]]], fmt,
      user_set = spec$user_set[[h]]
    )
  })
  names(sets) <- spec$headers
  return(sets)
}
