#' @keywords internal
#' @noRd
.implement_shock <- function(args_list,
                             class,
                             call) {
  args_list[["..."]] <- NULL
  if (class %=% "uniform") {
    if (length(args_list$subset) > 0) {
      if (length(args_list$subset) > 0 && (is.null(names(args_list$subset)) || any(names(args_list$subset) == ""))) {
        .cli_action(shk_err$uni_named_lst,
          shk_err$uni_named_lst,
          action = "abort"
        )
      }
    } else {
      args_list$subset <- NULL
    }

    shock <- list(
      var = args_list$var,
      input = args_list$value,
      subset = args_list$subset
    )
  } else {
    shock <- args_list
  }
  class(shock) <- c(class, class(shock))
  shock <- .validate_shock(
    shock = shock,
    call = call
  )
  return(shock)
}