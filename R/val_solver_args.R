#' @keywords internal
#' @noRd
.validate_solver_args <- function(a,
                                  paths,
                                  call,
                                  timeID = NULL) {
  a <- .validate_solver_methods(a, call = call)
  a <- .validate_solver_scalars(a, call = call)
  .validate_solver_steps(a, call = call)
  a$enable_time <- .solver_enable_time(paths = paths)
  a <- .solver_resources_record(a, paths = paths, call = call)
  a <- .solver_method_codes(a, call = call)
  return(a)
}
