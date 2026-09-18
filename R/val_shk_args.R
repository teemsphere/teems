#' @keywords internal
#' @noRd
.validate_shk <- function(shock,
                          call) {
  return(UseMethod(".validate_shk"))
}

#' @method .validate_shk uniform
#' @export
.validate_shk.uniform <- function(shock,
                                    call) {
  checklist <- list(
    var = "character",
    input = "numeric",
    subset = c("logical", "list")
  )

  .check_arg_class(
    args_list = shock,
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

  # might be able to reduce depth to an attribute
  shock <- structure(shock,
    call = call,
    class = c(depth, class(shock))
  )

  shock <- list(shock)
  return(shock)
}

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
  # element names are case-insensitive on input, lowercase inside TEEMS
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
