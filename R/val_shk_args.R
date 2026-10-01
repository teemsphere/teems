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

  .check_arg_class(
    args_list = shock,
    checklist = checklist,
    call = call
  )

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
