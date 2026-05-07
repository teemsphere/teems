#' Prepare closure swaps
#'
#' Prepares a partial or full variable swap for use as `swap_in`
#' or `swap_out` in [`ems_deploy()`].
#'
#' @param var Character of length 1, model variable to swap.
#' @param ... Optional named arguments restricting the swap to a
#'   subset of the variable's elements. Each name must use the
#'   model-specific set-index format (set name concatenated with
#'   its index letter, e.g., `REGr`, `COMMc`). Unspecified sets
#'   default to all elements. Three value forms are accepted:
#'   * Single element: `SETi = "element"`
#'   * Multiple elements: `SETi = c("element1", "element2")`
#'   * Subset: `SETi = "SUBSET"`
#'
#' @return A list of class `"full"` or`"partial"` and `"single"`
#'   or`"multi"` containing the swap specification. Intended for
#'   use as `swap_in` or `swap_out` in [`ems_deploy()`].
#'
#' @details `ems_swap()` return values have no purpose used in
#'   isolation. The closure provided to the `closure_file`
#'   argument of [`ems_model()`] will be used if no swaps are
#'   specified. Note that full variable swaps can be directly
#'   inputted as a character string in the `"swap_in"` and
#'   `"swap_out"` arguments of [`ems_deploy()`].
#'
#' @seealso [`ems_model()`] for loading a model closure.
#'   [`ems_deploy()`] for loading the output of this function.
#'
#' @examples
#' # Full variable swaps
#' tfd_out <- ems_swap(var = "tfd")
#' qfd_in <- ems_swap(var = "qfd")
#'
#' # Partial variable swaps with uniform "PROD_COMM" set
#' # application (note distinction between "REGr" and "REGs") in
#' # the classic GTAP model (version 6.2)
#' chn_food_tfd_out <- ems_swap(var = "tfd",
#'                              TRAD_COMMi = "food",
#'                              REGr = "chn")
#'
#' chn_food_qfd_in <- ems_swap(var = "qfd",
#'                             TRAD_COMMi = "food",
#'                             REGs = "chn")
#'
#' # Partial variable, multiple element swaps with uniform "ACTS"
#' # set application in the standard GTAP model (version 7.0).
#' usa_multi_tfd_out <- ems_swap(var = "tfd",
#'                               COMMc = c("food", "crops"),
#'                               REGr = "usa")
#'
#' usa_multi_qfd_in <- ems_swap(var = "qfd",
#'                              COMMc = c("food", "crops"),
#'                              REGr = "usa")
#'                              
#' # Valid subsets may also be selected.
#' qxs_in <- ems_swap(var = "qxs",
#'                    COMMc = "NMRG",
#'                    ALLTIMEt = "FWDTIME")
#'
#' txs_out <- ems_swap(var = "txs",
#'                     COMMc = "NMRG",
#'                     ALLTIMEt = "FWDTIME")
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
