#' Load uniform shock
#'
#' @description `ems_uniform_shock()` loads uniform shocks for
#'   processing as well as conducts a series of compatibility
#'   checks. A uniform shock is one which applies a homogeneous
#'   value across all or part of a variable (using `...`).  The
#'   accepted values for `...` depend on the `"var"` specified
#'   and set mappings associated with this variable. If a uniform
#'   shock is to be carried out, the output of this function is a
#'   required input to the `"shock"` argument within the
#'   [`ems_deploy()`] function.
#'
#' @param var Character of length 1, the variable to be shocked.
#' @param value Numeric length 1, value of uniform shock.
#' @param ... One or more variable-specific key-value pairs
#'   separated by commas corresponding to the parts of a variable
#'   that will receive a uniform shock.
#'
#' @seealso [`ems_deploy()`] for loading the output of this
#'   function.
#' @seealso [`ems_swap()`] for changing the standard model
#'   closure.
#' @examples
#' # fully uniform: all variable elements receive the same shock value
#' afeall_full <- ems_uniform_shock(var = "afeall",
#'                                  value = 2)
#'
#' # partially uniform: applied only to the "chn" element in set REGr (REG)
#' # Note that set designations must consist of the concatenation of the
#' # standard set (e.g., REG) and variable-specific index (e.g., r).
#' afeall_chn <- ems_uniform_shock(var = "afeall",
#'                                 REGr = "chn",
#'                                 value = 2)
#' @export
ems_uniform_shock <- function(var,
                              value,
                              ...)
{
args_list <- mget(names(formals()))
args_list$subset <- list(...)
call <- match.call()
shock <- .implement_shock(
  args_list = args_list,
  class = "uniform",
  call = call
)
shock
}