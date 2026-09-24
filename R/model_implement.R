#' @importFrom cli cli_verbatim
#' @keywords internal
#' @noRd
.implement_model <- function(args_list,
                             call) {

  v <- .validate_model_args(
    a = args_list,
    call = call
  )

  model <- .process_tablo(
    tab_file = v$model_file,
    backsolve = v$backsolve,
    ignore_condense = v$ignore_condense,
    extra_statements = attr(v$closure, "xsets"),
    call = call
  )

  if (.o_verbose()) {
    model_summary <- attr(model, "model_summary")
  }

  if (v$mod_coeff %!=% NA) {
    model <- .coeff_mod(
      mod_coeff = v$mod_coeff,
      model = model,
      call = call
    )
  }

  closure <- .closure_add_omitted(
    closure = v$closure,
    omit_vars = attr(model, "omit_vars")
  )

  closure <- .check_closure(
    closure = closure,
    var_extract = model[model$type == "Variable", ],
    call = call
  )

  if (.o_verbose()) {
    cli::cli_verbatim(model_summary)
  }
  
  model <- structure(model,
    closure = closure,
    closure_file = attr(v$closure, "file"),
    call = call
  )

  return(model)
}
