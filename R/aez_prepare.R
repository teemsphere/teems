#' @importFrom purrr map
#' @keywords internal
#' @noRd
.prepare_aez <- function(i_data, call) {
  spec <- layer_spec$aez
  attrs <- attributes(i_data)
  fmt <- attrs[["metadata"]][["data_format"]]
  nm <- toupper(names(i_data))
  .layer_require(i_data, spec, call = call)

  restrict <- .layer_elements(i_data, spec$restrict)
  elements <- purrr::map(spec$sets, \(s) {
    intersect(s$elements %|||% .layer_elements(i_data, s$header), restrict)
  })
  new_sets <- .layer_sets(spec, elements, fmt)
  new_sets <- new_sets[!names(new_sets) %in% nm]

  i_data <- .layer_rename(i_data, spec)

  new_par <- .layer_par(spec, i_data, fmt)
  new_par <- new_par[!names(new_par) %in% nm]

  prepared <- .layer_finish(i_data, new_sets, new_par,
    attrs = attrs, flag = spec$flag
  )
  return(prepared)
}