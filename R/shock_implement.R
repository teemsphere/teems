#' @keywords internal
#' @noRd
.implement_shock <- function(args_list,
                             class,
                             call) {
  args_list[["..."]] <- NULL
  if (class %=% "uniform") {
    args_list$subset <- .check_named_dots(args_list$subset)

    if (isFALSE(args_list$subset)) {
      .cli_action(shk_err$uni_named_lst,
        action = c("abort", "inform", "inform"),
        call = call
      )
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