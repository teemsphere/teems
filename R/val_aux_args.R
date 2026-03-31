#' @importFrom rlang arg_match
#' 
#' @keywords internal
#' @noRd
.validate_aux_args <- function(a,
                               call) {
  
  type <- a$type
  a$type <- rlang::arg_match(
    arg = type,
    values = c("dat", "par", "set"),
    error_call = call
  )
  
  checklist <- list(
    input = c("character", "data.frame"),
    type = c("character"),
    header = c("NULL", "character")
  )

  .check_arg_class(
    args_list = a,
    checklist = checklist,
    call = call
  )

  if (is.character(a$input) && length(a$input) %=% 1L) {
    a$input <- .check_input(
      file = a$input,
      valid_ext = c("csv", "har"),
      call = call
    )
  }

  if (!inherits(a$input, "har") && is.null(a$header)) {
    .cli_action(aux_err$missing_header,
      action = "abort",
      call = call
    )
  }

  return(a)
}