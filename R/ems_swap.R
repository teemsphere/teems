#' Load swaps
#'
#' @description `ems_swap()` is a helper function that loads
#'   swaps, allowing for a change of endogenous/exogenous
#'   variable status. If a swap is specified using this function,
#'   the output is a required input to the `"swap_in"` or
#'   `"swap_out"` arguments of the [`ems_deploy()`] function.
#'
#' @param var Character of length 1, model variable to swap.
#' @param ... One or more key-value pairs separated by commas.
#'   These correspond to element- or subset-specific swaps in the
#'   format SETi = "component" or SETi = c("component1",
#'   "component2") where i denotes the variable-specific index
#'   (see examples). Sets not specified are implicitly determined
#'   as uniform.
#'
#' @return A list with swap specifications.
#'
#' @details `ems_swap()` return values have no purpose used in
#'   isolation. The standard model-specific closure will be used
#'   if no swaps are specified. Note that full variable swaps can
#'   be directly inputted as a character string in
#'   [`ems_deploy()`].
#'
#' @seealso [`ems_uniform_shock()`] for specification of uniform
#'   shocks.
#' @seealso [`ems_custom_shock()`] for specification of custom
#'   shocks.
#' @seealso [`ems_scenario_shock()`] for specification of
#'   scenario shocks.
#' @seealso [`ems_deploy()`] for loading the output of this
#'   function.
#' 
#' @examples
#' # Full variable swaps
#' tfd_out <- ems_swap(var = "tfd")
#' qfd_in <- ems_swap(var = "qfd")
#'
#' # Partial variable swaps (note distinction between "REGr" and "REGs")
#' chn_food_tfd_out <- ems_swap(var = "tfd",
#'                              TRAD_COMMi = "food",
#'                              REGr = "chn")
#'                                
#' chn_food_qfd_in <- ems_swap(var = "qfd",
#'                             TRAD_COMMi = "food",
#'                             REGs = "chn")
#'
#' # Partial variable multiple element swaps
#' usa_multi_tfd_out <- ems_swap(var = "tfd",
#'                               TRAD_COMMi = c("food", "crops"),
#'                               REGr = "usa")
#'                                
#' usa_multi_qfd_in <- ems_swap(var = "qfd",
#'                              TRAD_COMMi = c("food", "crops"),
#'                              REGs = "usa")
#'                               
#' @export
ems_swap <- function(var,
                     ...
) {
  if (missing(var)) {
    .cli_missing(var)
  }
args_list <- mget(names(formals()))
args_list$subset <- list(...)
call <- match.call()
swap <- .implement_swap(
  args_list = args_list,
  call = call
)
swap
}
