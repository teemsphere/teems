#' @importFrom purrr map_lgl
#' @keywords internal
#' @noRd
.finalize <- function(args_list,
                      call) {
  metadata <- attr(args_list$.data, "metadata")
  attr(metadata, "file") <- "metadata.rds"
  data_call <- attr(args_list$.data, "call")
  model_call <- attr(args_list$model, "call")
  var_extract <- args_list$model[
    args_list$model$type == "Variable" & is.na(args_list$model$condense),
  ]
  sets <- .finalize_sets(
    sets = args_list$.data[purrr::map_lgl(args_list$.data, inherits, "set")],
    set_extract = args_list$model[args_list$model$type == "Set", ],
    coeff_extract = args_list$model[args_list$model$type == "Coefficient", ],
    time_steps = attr(args_list$.data, "time_steps"),
    reference_year = metadata$reference_year,
    call = call,
    data_call = data_call,
    model_call = model_call,
    coeff_data = args_list$.data[!purrr::map_lgl(args_list$.data, inherits, "set")],
    model = args_list$model,
    set_raw = attr(args_list$.data, "set_raw")
  )
  sb_data <- attr(sets, "sb_data")
  if (length(sb_data) > 0L) {
    args_list$.data[names(sb_data)] <- sb_data
  }
  .check_subset_containment(
    sets = sets,
    call = model_call
  )
  v <- .validate_deploy_args(
    a = args_list,
    sets = sets,
    call = call,
    data_call = data_call
  )
  closure <- .finalize_closure(
    closure = attr(v$model, "closure"),
    closure_file = attr(v$model, "closure_file"),
    swap_in = v$swap_in,
    swap_out = v$swap_out,
    sets = sets,
    var_extract = var_extract,
    call = call,
    model_call = model_call
  )
  n_comp_active <- .comp_active_count(
    model = v$model,
    closure = closure,
    var_extract = var_extract,
    sets = sets,
    call = call
  )
  size_metadata <- .compute_size_metadata(
    var_extract = var_extract,
    sets = sets,
    closure = closure
  )
  .check_system_square(
    model = v$model,
    var_extract = var_extract,
    sets = sets,
    closure = closure,
    size_metadata = size_metadata,
    n_comp_active = n_comp_active,
    call = call
  )
  metadata$vpqtype <- attr(v$model, "vpqtype")
  metadata$system_size <- size_metadata$system_size
  metadata$n_var_ele <- size_metadata$n_var_ele
  metadata$n_exo_ele <- size_metadata$n_exo_ele
  metadata$n_reg <- size_metadata$n_reg
  time_steps <- attr(args_list$.data, "time_steps")
  metadata$n_time <- if (is.null(time_steps)) {
    0L
  } else {
    length(time_steps)
  }
  metadata$time_steps <- time_steps
  metadata$condense <- .compute_cndns_metadata(
    model = v$model,
    sets = sets,
    system_size = size_metadata$system_size
  )
  shocks <- .finalize_shks(
    shock = v$shock,
    closure = closure,
    sets = sets,
    var_extract = var_extract
  )
  .data <- .finalize_data(
    .data = v$.data,
    sets = sets,
    model = v$model,
    call = call,
    model_call = model_call
  )
  .data <- c(.data, .finalize_map_data(
    model = v$model,
    sets = sets,
    set_raw = attr(args_list$.data, "set_raw"),
    call = call,
    data_call = data_call
  ))
  map_names <- v$model$name[v$model$type == "Mapping"]
  metadata$mapped_equations <- length(map_names) > 0L &&
    any(grepl(
      paste0("\\b(", paste(tolower(map_names), collapse = "|"), ")\\s*\\("),
      tolower(v$model$tab[v$model$type == "Equation"])
    ))
  tab <- .finalize_tab(
    model = v$model,
    write_coefficients = v$write_coefficients
  )
  cmf <- .finalize_cmf(
    model = v$model,
    shock_file = attr(shocks, "file"),
    tab_file = attr(tab, "file"),
    cls_file = attr(closure, "file"),
    write_coefficients = v$write_coefficients
  )
  cmf_path <- .write_input_files(
    tab = tab,
    closure = closure,
    shocks = shocks,
    cmf = cmf,
    .data = .data,
    v_shock = v$shock,
    metadata = metadata,
    sets = sets
  )
  return(cmf_path)
}
