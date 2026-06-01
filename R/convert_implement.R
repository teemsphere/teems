#' @importFrom purrr map_lgl
#'
#' @keywords internal
#' @noRd
.implement_convert <- function(args_list,
                               call) {
  v <- .validate_convert_args(
    a = args_list,
    call = call
  )

  i_data <- .load_input_data(
    dat_input = v$dat_input,
    par_input = v$par_input,
    set_input = v$set_input,
    data_call = call,
    convert = TRUE
  )

  if (attr(i_data, "metadata")$data_format %=% v$target) {
    target <- v$target
    .cli_action(convert_wrn$format,
      action = "warn",
      call = call
    )

    v$target <- NULL
  }

  if (!is.null(v$target)) {
    i_data <- .convert_data(i_data = i_data)
  }

  dat <- i_data[purrr::map_lgl(i_data, \(i) {
    inherits(i, "dat")
  })]
  attr(dat, "metadata") <- attr(i_data, "metadata")

  par <- i_data[purrr::map_lgl(i_data, \(i) {
    inherits(i, "par")
  })]

  set <- i_data[purrr::map_lgl(i_data, \(i) {
    inherits(i, "set")
  })]

  i_data <- list(
    dat = dat,
    par = par,
    set = set
  )
  return(i_data)
}