#' @keywords internal
#' @noRd
.inject_value <- function(input,
                          model,
                          call) {
  UseMethod(".inject_value")
}

#' @keywords internal
#' @noRd
#' @method .inject_value numeric
#' @export
.inject_value.numeric <- function(input,
                                  model,
                                  call) {
  model <- .tab_mod(
    input = input,
    model = model,
    call = call
  )
}

#' @keywords internal
#' @noRd
#' @method .inject_value data.frame_read
#' @export
.inject_value.data.frame_read <- function(input,
                                          model,
                                          call) {
  nme <- attr(input, "name")
  input <- .inject_v_df(
    input = input,
    model = model,
    call = call
  )

  attr(model, nme) <- input
  return(model)
}

#' @keywords internal
#' @noRd
#' @method .inject_value default
#' @export
.inject_value.default <- function(input,
                                  model,
                                  call) {
  model <- .inject_v_char(
    input = input,
    model = model,
    call = call
  )

  return(model)
}

#' @keywords internal
#' @noRd
#' @method .inject_value data.frame_formula
#' @export
.inject_value.data.frame_formula <- function(input,
                                             model,
                                             call) {
  nme <- attr(input, "name")
  input <- .inject_v_df(
    input = input,
    model = model,
    call = call
  )

  model <- .tab_mod(
    input = input,
    model = model,
    call = call
  )

  header_attr <- attr(model, "header")
  header_attr <- c(header_attr, nme)

  attr(model, "header") <- header_attr
  attr(model, nme) <- input
  return(model)
}