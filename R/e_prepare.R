#' @keywords internal
#' @noRd
.prepare_e <- function(i_data, call) {
  spec <- layer_spec$e
  attrs <- attributes(i_data)
  fmt <- attrs[["metadata"]][["data_format"]]
  nm <- toupper(names(i_data))
  .layer_require(i_data, spec, call = call)

  comm <- .layer_elements(i_data, "COMM")
  come <- .layer_elements(i_data, "COME")
  fuel <- .layer_elements(i_data, "FUEL")

  elements <- list(
    DCOM = comm,
    MCOM = comm,
    DELY = setdiff(come, fuel),
    EGY = come,
    ENY = come,
    TOPP = c(spec$vocab$energy_top, setdiff(comm, come))
  )
  new_sets <- .layer_sets(spec, elements, fmt)
  new_sets <- new_sets[!names(new_sets) %in% nm]

  i_data <- .layer_reclass_blocs(i_data, spec, fmt)
  i_data <- .layer_cde(i_data, spec, topp = elements$TOPP, call = call)
  prepared <- .layer_finish(i_data, new_sets, attrs = attrs, flag = spec$flag)
  return(prepared)
}
