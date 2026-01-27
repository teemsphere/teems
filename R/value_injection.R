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

#' @importFrom purrr pluck
#' @importFrom data.table setcolorder as.data.table setkeyv
#' @keywords internal
#' @noRd
#' @method .inject_value data.frame_read
#' @export
.inject_value.data.frame_read <- function(input,
                                          model,
                                          call) {
  nme <- attr(input, "name")
  req_col <- c(purrr::pluck(model, "ls_mixed_idx", nme), "Value")
  input_col <- colnames(input)

  if (!all(req_col %in% input_col)) {
    .cli_action(load_err$injection_missing_col,
      action = c("abort", "inform"),
      call = call
    )
  }

  input[] <- lapply(input, function(c) {
    if (is.factor(c)) {
      as.character(c)
    } else {
      c
    }
  })

  if (!inherits(input, "data.table")) {
    input <- data.table::as.data.table(input)
  }

  if (!req_col %=% input_col) {
    data.table::setcolorder(input, req_col)
  }

  data.table::setkeyv(input, setdiff(req_col, "Value"))
  attr(model, nme) <- input
  return(model)
}

#' @importFrom purrr pluck
#' @importFrom data.table fread
#' @keywords internal
#' @noRd
#' @method .inject_value character_read
#' @export
.inject_value.character_read <- function(input,
                                         model,
                                         call) {
  nme <- attr(input, "name")
  input <- .check_input(
    file = input,
    valid_ext = "csv",
    call = call
  )

  input <- data.table::fread(input)
  attr(input, "name") <- nme
  concat_class <- class(input)[length(class(input))]
  class(input) <- c(paste0(concat_class, "_read"), class(input))
  model <- .inject_value(
    input = input,
    model = model,
    call = call
  )

  return(model)
}

#' @importFrom purrr pluck
#' @importFrom data.table fread
#' @keywords internal
#' @noRd
#' @method .inject_value character_formula
#' @export
.inject_value.character_formula <- function(input,
                                            model,
                                            call) {
  nme <- attr(input, "name")
  input <- .check_input(
    file = input,
    valid_ext = "csv",
    call = call
  )

  input <- data.table::fread(input)
  attr(input, "name") <- nme
  concat_class <- class(input)[length(class(input))]
  class(input) <- c(paste0(concat_class, "_formula"), class(input))
  model <- .inject_value(
    input = input,
    model = model,
    call = call
  )
  return(model)
}

#' @importFrom data.table setcolorder as.data.table setkeyv
#' @keywords internal
#' @noRd
#' @method .inject_value data.frame_formula
#' @export
.inject_value.data.frame_formula <- function(input,
                                             model,
                                             call) {
  nme <- attr(input, "name")
  req_col <- c(purrr::pluck(model, "ls_mixed_idx", nme), "Value")
  input_col <- colnames(input)

  if (!all(req_col %in% input_col)) {
    .cli_action(load_err$injection_missing_col,
      action = c("abort", "inform"),
      call = call
    )
  }

  input[] <- lapply(input, function(c) {
    if (is.factor(c)) {
      as.character(c)
    } else {
      c
    }
  })

  if (!inherits(input, "data.table")) {
    input <- data.table::as.data.table(input)
  }

  if (!req_col %=% input_col) {
    data.table::setcolorder(input, req_col)
  }

  data.table::setkeyv(input, setdiff(req_col, "Value"))
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