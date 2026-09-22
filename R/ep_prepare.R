#' @keywords internal
#' @noRd
.prepare_ep <- function(i_data, call) {
  spec <- layer_spec$ep
  v <- spec$vocab
  attrs <- attributes(i_data)
  fmt <- attrs[["metadata"]][["data_format"]]
  nm <- toupper(names(i_data))
  .layer_require(i_data, spec, call = call)

  comm <- .layer_elements(i_data, "COMM")
  fuel <- .layer_elements(i_data, "FUEL")
  elec <- .layer_elements(i_data, "ELEC")
  techs <- .layer_elements(i_data, "ELEA")

  split <- .ep_load_split(techs)
  if (is.null(split)) {
    ep_techs <- techs
    .cli_action(data_err$ep_load_split,
      action = "abort",
      call = call
    )
  }

  egy <- union(fuel, elec)
  elements <- list(
    DCOM = comm,
    MCOM = comm,
    DELY = elec,
    EGY = egy,
    TOPP = c(v$energy_top, setdiff(comm, egy)),
    ENY = c(fuel, v$electricity),
    ELE = c(v$electricity, v$generation, v$base_load, v$peak_load),
    ELY = c(v$generation, setdiff(elec, techs)),
    EGN = c(v$base_load, v$peak_load),
    EBL = split$base,
    EPL = split$peak
  )
  new_sets <- .layer_sets(spec, elements, fmt)
  new_sets <- new_sets[!names(new_sets) %in% nm]

  i_data <- .layer_reclass_blocs(i_data, spec, fmt)
  i_data <- .layer_promote_cde(i_data, spec, call = call)
  prepared <- .layer_finish(i_data, new_sets, attrs = attrs, flag = spec$flag)
  return(prepared)
}
