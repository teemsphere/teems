#' @title Prepare a uniform shock
#' @export
#' @description Prepares a uniform shock to be implemented as a
#'   single percentage-change value uniformly across all or a
#'   subset of variable tuples.
#' @return A `list` object with shock configuration to be passed
#'   to the `shock` argument of [`ems_deploy()`].
#' @param var Character of length 1, the variable to be shocked.
#' @param value Numeric length 1, percentage-change value of the
#'   shock.
#' @param ... Optional named arguments restricting the shock to a
#'   subset of the variable's elements. Each name must use the
#'   model-specific set-index format (set name concatenated with
#'   its index letter, e.g., `REGr`, `COMMc`). Value may consist
#'   of one or more elements or a subset. When omitted, the shock
#'   is applied to all variable tuples.
#' @seealso [`ems_deploy()`] for loading the output of this
#'   function. [`ems_swap()`] for changing the standard model
#'   closure.
#' @examples
#' # Full uniform: 2% shock imposed on all afeall tuples
#' ems_uniform_shock(var = "afeall", value = 2)
#'
#' # Partial uniform by element: applied only to the "chn"
#' # element in set REGr (REG). Note that set designations must
#' # consist of the concatenation of the standard set (e.g., REG)
#' # and variable-specific index (e.g., r).
#' ems_uniform_shock(var = "afeall",
#'                   REGr = "chn",
#'                   value = 2)
#'
#' # Partially uniform by subset and element: applied to "chn"
#' # and "usa" across the MARG and FWDTIME subsets.
#' ems_uniform_shock(var = "qxs",
#'                   COMMc = "MARG",
#'                   REGs = c("chn", "usa"),
#'                   ALLTIMEt = "FWDTIME",
#'                   value = -1)
ems_uniform_shock <- function(var,
                              value,
                              ...)
{
if (missing(var)) {
  .cli_missing(var)
}
if (missing(value)) {
  .cli_missing(value)
}
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