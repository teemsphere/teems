#' @title Prepare closure swaps
#' @description Prepares a closure swap for input into the
#'   `swap_in` or `swap_out` arguments of [`ems_deploy()`].
#' @export
#' @param var Character of length 1, model variable to swap.
#' @param ... Optional named arguments restricting the swap to a
#'   subset of variable elements. Each name must use the
#'   model-specific set-index format (set name concatenated with
#'   its index letter, e.g., `REGr`, `COMMc`). Unspecified sets
#'   default to all elements. `SET` corresponds to the generic
#'   set name while `i` is specific to the variable index. Three
#'   forms are accepted:
#'   * Single element: `SETi = "element"`
#'   * Multiple elements: `SETi = c("element1", "element2")`
#'   * Subset: `SETi = "SUBSET"`
#' @return A list of class `"full"` or `"partial"` and `"single"`
#'   or `"multi"` containing the swap specification. Intended for
#'   use as `swap_in` or `swap_out` in [`ems_deploy()`].
#' @details `ems_swap()` return values have no purpose used in
#'   isolation. The closure provided to the `closure_file`
#'   argument of [`ems_model()`] will be used if no swaps are
#'   specified. Note that full variable swaps can be directly
#'   inputted as a character string in the `"swap_in"` and
#'   `"swap_out"` arguments of [`ems_deploy()`].
#'
#'   Swaps "in" (joining the list of exogenous variables) are
#'   processed prior to swaps out (leaving the list of exogenous
#'   variables), and in the same order as they are loaded into
#'   [`ems_deploy()`].
#' @seealso [`ems_model()`] for parsing and modifying model and
#'   closure files. [`ems_deploy()`] for loading the output of
#'   this function.
#' @examples
#' # Full variable swaps
#' ems_swap("tfd") # out
#' ems_swap("qfd") # in
#'
#' # Partial variable swaps with uniform "PROD_COMM" set
#' # application (note distinction between "REGr" and "REGs") in
#' # the classic GTAP model (version 6.2)
#' ems_swap("tfd", TRAD_COMMi = "food", REGr = "chn") # out
#' ems_swap("qfd", TRAD_COMMi = "food", REGs = "chn") # in
#'
#' # Partial variable, multiple element swaps with uniform "ACTS"
#' # set application in the standard GTAP model (version 7.0).
#' ems_swap("tfd", COMMc = c("food", "crops"), REGr = "usa") # out
#' ems_swap("qfd", COMMc = c("food", "crops"), REGr = "usa") # in
#'
#' # Valid subsets may also be selected.
#' ems_swap("txs", COMMc = "NMRG", ALLTIMEt = "FWDTIME") # out
#' ems_swap("qxs", COMMc = "NMRG", ALLTIMEt = "FWDTIME") # in
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
