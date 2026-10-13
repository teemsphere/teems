#' @importFrom rlang arg_match
#' @keywords internal
#' @noRd
.validate_exp_args <- function(a,
                               checklist,
                               call) {
  type <- a$type
  a$type <- rlang::arg_match(
    type,
    c("model_files", "scripts"),
    error_call = call
  )

  checklist <- list(
    model = "character",
    path = "character",
    type = "character",
    dat_input = c("NULL", "character", "list"),
    par_input = c("NULL", "character", "list"),
    set_input = c("NULL", "character", "list")
  )

  .check_arg_class(
    args_list = a,
    checklist = checklist,
    call = call
  )

  model <- a$model
  a$model <- rlang::arg_match(
    model,
    basename(list.dirs(system.file("models", package = "teems"), recursive = FALSE)),
    error_call = call
  )

  a <- .data_inputs(a = a, call = call)

  if (!dir.exists(a$path) || file.access(a$path, 2) != 0L) {
    path <- a$path
    .cli_action(exp_err$invalid_path,
      action = "abort",
      call = call
    )
  }
  
  a$path <- normalizePath(a$path, "/")
  
  if (a$type %=% "scripts") {
    if (is.null(a$dat_input) || is.null(a$par_input) ||
      is.null(a$set_input)) {
      .cli_action(exp_err$missing_input,
        action = "abort",
        call = call
      )
    }

    for (nm in c("dat_input", "par_input", "set_input")) {
      if (is.character(a[[nm]])) {
        a[[nm]] <- normalizePath(a[[nm]], "/", FALSE)
      }
    }
  }

  return(a)
}
