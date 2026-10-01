#' @importFrom purrr map_lgl
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
    data_call = call,
    generic = v$generic
  )
  if (.layer_detect(i_data, "aez") && !isTRUE(attr(i_data, "metadata")[["aez"]])) {
    i_data <- .prepare_aez(i_data = i_data, call = call)
    if (.o_verbose()) {
      .cli_action(data_info$aez,
        action = "inform",
        call = call
      )
    }
  }

  if (.layer_detect(i_data, "e") && !isTRUE(attr(i_data, "metadata")[["e"]])) {
    i_data <- .prepare_e(i_data = i_data, call = call)
    if (.o_verbose()) {
      .cli_action(data_info$e,
        action = "inform",
        call = call
      )
    }
  }

  if (.layer_detect(i_data, "ep") && !isTRUE(attr(i_data, "metadata")[["ep"]])) {
    i_data <- .prepare_ep(i_data = i_data, call = call)
    if (.o_verbose()) {
      .cli_action(data_info$ep,
        action = "inform",
        call = call
      )
    }
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
    par_weights = v$par_weights,
    call = call
  )
  
  class(i_data) <- c("ems_data", class(i_data))
  return(i_data)
}
