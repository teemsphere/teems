#' @importFrom tools R_user_dir
#' @importFrom rlang arg_match
#'
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
    path = "character",
    model = "character",
    type = "character",
    dat_input = c("NULL", "character"),
    par_input = c("NULL", "character"),
    set_input = c("NULL", "character")
  )

  .check_arg_class(
    args_list = a,
    checklist = checklist,
    call = call
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
    if (any(
      is.null(a$dat_input),
      is.null(a$par_input),
      is.null(a$set_input)
    )) {
      .cli_action(exp_err$missing_input,
        action = "abort",
        call = call
      )
    }
    a$dat_input <- normalizePath(a$dat_input, "/", FALSE)
    a$par_input <- normalizePath(a$par_input, "/", FALSE)
    a$set_input <- normalizePath(a$set_input, "/", FALSE)
  }

  return(a)
}