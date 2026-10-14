#' @importFrom rlang is_integerish is_logical
#' @keywords internal
#' @noRd
.validate_probe_args <- function(a,
                                 call) {
  xtr_args <- .solver_extra_args()
  dots <- a$solver_extras
  unknown_args <- setdiff(names(dots), names(xtr_args))
  if (length(dots) &&
    (is.null(names(dots)) || !all(nzchar(names(dots))) || length(unknown_args))) {
    if (!length(unknown_args)) {
      unknown_args <- "<unnamed>"
    }
    .cli_action(probe_err$probe_dots,
      action = c("abort", "inform"),
      call = call
    )
  }
  for (nm in names(dots)) {
    xtr_args[nm] <- dots[nm]
  }
  .validate_solver_extras(a = xtr_args, call = call)
  a$solver_extras <- xtr_args
  if (!rlang::is_logical(a$components, n = 1) || is.na(a$components)) {
    arg <- "components"
    .cli_action(probe_err$x_logical,
      action = "abort",
      call = call
    )
  }
  if (!is.null(a$cores) &&
    (!rlang::is_integerish(a$cores) || length(a$cores) != 1L || is.na(a$cores) || a$cores < 1)) {
    arg <- "cores"
    .cli_action(probe_err$x_positive,
      action = "abort",
      call = call
    )
  }
  if (!is.null(a$memory) &&
    (!is.numeric(a$memory) || length(a$memory) != 1L || is.na(a$memory) || a$memory <= 0)) {
    arg <- "memory"
    .cli_action(probe_err$x_positive,
      action = "abort",
      call = call
    )
  }
  return(a)
}
