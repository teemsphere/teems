#' @importFrom purrr map2
#' 
#' @keywords internal
#' @noRd
.implement_exp <- function(args_list,
                           call) {

  v <- .validate_exp_args(
    a = args_list,
    call = call
  )

  model_file <- paste0(v$model, ".tab")
  closure_file <- paste0(v$model, ".cls")
  origin <- system.file(file.path("models", v$model, c(model_file, closure_file)), package = "teems", mustWork = TRUE)
  file.copy(
    from = origin,
    to = v$write_dir
  )
  paths <- file.path(v$write_dir, c(model_file, closure_file))
  names(paths) <- c("model_file", "closure_file")

  if (v$type %=% "scripts") {
    dir <- system.file(file.path("scripts", v$model), package = "teems", mustWork = TRUE)
    scripts <- list.files(dir, full.names = TRUE)
    templates <- lapply(scripts, readLines)
    paths <- purrr::map2(templates,
      scripts,
      .inject_script,
      dat_input = v$dat_input,
      par_input = v$par_input,
      set_input = v$set_input,
      target_format = v$target_format,
      write_dir = v$write_dir,
      model_file = paths[["model_file"]],
      closure_file = paths[["closure_file"]]
    )
  }
  return(paths)
}