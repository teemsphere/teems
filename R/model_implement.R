#' @importFrom cli cli_verbatim
#' 
#' @keywords internal
#' @noRd
.implement_model <- function(...,
                             args_list,
                             call) {
  checklist <- list(
    model_input = "character",
    closure_file = "character",
    var_omit = c("NULL", "character")
  )

  args_list$... <- NULL
  v <- .validate_model_args(
    a = args_list,
    checklist = checklist,
    call = call
  )

  model <- .process_tablo(
    tab_file = v$model_input,
    var_omit = v$var_omit,
    call = call
  )

  if (.o_verbose()) {
    model_summary <- attr(model, "model_summary")
  }

  if (length(list(...)) > 0L) {
    mod_coeff <- list(...)
    model <- .coeff_mod(
      mod_coeff = mod_coeff,
      model = model,
      call = call
    )
  }

  closure <- .check_closure(
    closure = v$closure,
    var_omit = v$var_omit,
    var_extract = model[model$type == "Variable", ],
    call - call
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