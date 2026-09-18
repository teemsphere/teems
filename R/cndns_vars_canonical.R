# Case-insensitive canonicalization of user/TAB variable names.
#' @keywords internal
#' @noRd
.canonical_vars <- function(input,
                            var_extract,
                            err,
                            call) {
  if (length(input) == 0L) {
    vars <- character()
    return(vars)
  }
  r_idx <- match(tolower(input), tolower(var_extract$name))
  if (anyNA(r_idx)) {
    invalid_var <- input[is.na(r_idx)]
    .cli_action(err,
      action = "abort",
      call = call
    )
  }
  return(var_extract$name[r_idx])
}
