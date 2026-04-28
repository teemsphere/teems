#' @importFrom purrr map_lgl
#' 
#' @keywords internal
#' @noRd
.finalize <- function(args_list,
                      call) {
  metadata <- attr(args_list$.data, "metadata")
  attr(metadata, "file") <- "metadata.rds"
  data_call <- attr(args_list$.data, "call")
  model_call <- attr(args_list$model, "call")
  var_extract <- args_list$model[args_list$model$type == "Variable", ]
  sets <- .finalize_sets(
    sets = args_list$.data[purrr::map_lgl(args_list$.data, inherits, "set")],
    set_extract = args_list$model[args_list$model$type == "Set", ],
    coeff_extract = args_list$model[args_list$model$type == "Coefficient", ],
    time_steps = attr(args_list$.data, "time_steps"),
    reference_year = metadata$reference_year,
    call = call,
    data_call = data_call,
    model_call = model_call
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
  shocks <- .finalize_shocks(
    shock = v$shock,
    shock_file = v$shock_file,
    closure = closure,
    sets = sets,
    var_extract = var_extract
  )
  .data <- .finalize_data(
    .data = v$.data,
    sets = sets,
    model = v$model,
    write_dir = v$write_dir,
    call = call,
    model_call = model_call
  )
  tab <- .finalize_tab(model = v$model)
  cmf <- .finalize_cmf(
    model = v$model,
    shock_file = attr(shocks, "file"),
    tab_file = attr(tab, "file"),
    cls_file = attr(closure, "file"),
    write_dir = v$write_dir
  )
  cmf_path <- .write_input_files(
    write_dir = v$write_dir,
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