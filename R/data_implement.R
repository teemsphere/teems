#' @importFrom purrr map_lgl
#' 
#' @keywords internal
#' @noRd
.implement_data <- function(args_list,
                            call) {
  v <- .validate_data_args(
    a = args_list,
    call = call
  )
  i_data <- .load_input_data(
    dat_input = v$dat_input,
    par_input = v$par_input,
    set_input = v$set_input,
    aux_input = v$aux_input,
    target_format = v$target_format,
    call = call
  )

  if (!is.null(v$target_format)) {
    i_data <- .convert_data(i_data = i_data)
  }
  
  set_mappings <- .load_mappings(
    set_mappings = v$set_mappings,
    set_data = i_data[purrr::map_lgl(i_data, inherits, "set")],
    time_steps = v$time_steps,
    metadata = attr(i_data, "metadata"),
    call = call
  )

  i_data <- .process_data(
    i_data = i_data,
    set_mappings = set_mappings,
    call = call
  )
  return(i_data)
}