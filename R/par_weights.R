#' @keywords internal
#' @noRd
.check_par_weights <- function(par_weights,
                               call) {
  invalid <- is.na(par_weights) | !par_weights %in% c("share", "value")
  if (any(invalid)) {
    e_method <- unique(par_weights[invalid])
    .cli_action(data_err$par_weights_method, action = "abort", call = call)
  }
  nm <- names(par_weights) %|||% rep("", length(par_weights))
  defaults <- par_weights[!nzchar(nm)]
  if (length(defaults) > 1L) {
    e_method <- defaults
    .cli_action(data_err$par_weights_default, action = c("abort", "inform"), call = call)
  }
  return(invisible(NULL))
}

#' @importFrom stats setNames
#' @keywords internal
#' @noRd
.resolve_par_weights <- function(par_weights,
                                 data_format,
                                 call) {
  weighted <- union(
    names(param_weights$value[[data_format]]),
    names(param_weights$share[[data_format]])
  )
  nm <- names(par_weights) %|||% rep("", length(par_weights))
  named <- par_weights[nzchar(nm)]
  unknown <- setdiff(names(named), weighted)
  if (length(unknown) > 0L) {
    e_header <- unknown
    e_format <- data_format
    e_weighted <- weighted
    .cli_action(data_err$par_weights_header, action = c("abort", "inform"), call = call)
  }
  default <- par_weights[!nzchar(nm)]
  if (length(default) %=% 0L) {
    default <- "share"
  }
  methods <- stats::setNames(rep(unname(default), length(weighted)), weighted)
  methods[names(named)] <- unname(named)
  return(methods)
}
