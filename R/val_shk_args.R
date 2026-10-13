#' @keywords internal
#' @noRd
.validate_shk <- function(shock,
                          call) {
  UseMethod(".validate_shk")
}

#' @method .validate_shk uniform
#' @export
.validate_shk.uniform <- function(shock,
                                    call) {
  checklist <- list(
    var = "character",
    value = "numeric",
    subset = c("logical", "list")
  )

  .check_arg_class(
    args_list = list(var = shock$var, value = shock$input, subset = shock$subset),
    checklist = checklist,
    call = call
  )

  if (length(shock$input) %!=% 1L || !is.finite(shock$input)) {
    value <- shock$input
    .cli_action(shk_err$uni_value,
      action = "abort",
      call = call
    )
  }

  if (shock$subset %!=% NA) {
    if (any(lengths(shock$subset) > 1)) {
      depth <- "multi"
    } else {
      depth <- "single"
    }
  } else {
    shock$subset <- NULL
    depth <- "single"
  }

  shock <- structure(shock,
    call = call,
    class = c(depth, class(shock))
  )

  shock <- list(shock)
  return(shock)
}

#' @importFrom data.table set
#' @method .validate_shk default
#' @export
.validate_shk.default <- function(shock,
                                    call) {
  checklist <- list(
    var = "character",
    input = c("character", "data.frame")
  )
  if ("partial" %in% names(shock)) {
    checklist$partial <- "logical"
  }

  .check_arg_class(
    args_list = shock,
    checklist = checklist,
    call = call
  )

  if ("partial" %in% names(shock) && (length(shock$partial) != 1L || is.na(shock$partial))) {
    bad_arg <- "partial"
    .cli_action(gen_err$logical_flag,
      action = "abort",
      call = call
    )
  }

  shock$input <- .shk_preload(
    input = shock$input,
    type = class(shock)[[1]],
    call = call
  )

  shock$set <- colnames(shock$input)[!colnames(shock$input) %in% "Value"]
  for (col in shock$set) {
    if (is.character(shock$input[[col]])) {
      data.table::set(shock$input, j = col, value = tolower(shock$input[[col]]))
    }
  }
  shock <- structure(shock,
    call = call,
    class = class(shock)
  )
  shock <- list(shock)
  return(shock)
}
