#' @keywords internal
#' @noRd
.implement_exp <- function(args_list,
                           call) {
  v <- .validate_exp_args(
    a = args_list,
    call = call
  )

  paths <- .get_model(
    model = v$model,
    path = v$path
  )

  if (v$type %=% "scripts") {
    paths <- .get_scripts(
      path = v$path,
      model = v$model,
      model_paths = paths,
      dat_input = v$dat_input,
      par_input = v$par_input,
      set_input = v$set_input,
      call = call
    )
  }

  return(paths)
}