#' Prepare a custom shock
#'
#' @description Prepares heterogeneous percentage-change shocks
#'   specified on a tuple-by-tuple basis. Only the tuples present
#'   in `input` are shocked.
#'
#' @inheritParams ems_uniform_shock
#' @param input Path to a CSV file, or a data frame or data frame
#'   extension (e.g., tibble, data.table). Must contain one
#'   column named `"Value"` with the percentage-change shock for
#'   each tuple, plus one column per set index for the variable.
#'   Note that set names must conform to the model-specific
#'   hybrid format (e.g., `ACTSa`, `REGr`). Column order is
#'   inconsequential.
#'
#' @return A `list` object with shock configuration to be passed
#'   to the `shock` argument of [`ems_deploy()`].
#'
#' @seealso [`ems_deploy()`] for loading the output of this
#'   function, [`ems_swap()`] for changing the standard model
#'   closure.
#' @examples
#' sectors <- c("crops", "food", "livestock", "mnfcs", "svces")
#' regions <- c("asia", "eit", "lam", "maf", "oecd")
#' time_steps <- 0:5
#'
#' aoall <- expand.grid(ACTSa = sectors,
#'                      REGr = regions,
#'                      ALLTIMEt = time_steps,
#'                      stringsAsFactors = FALSE)
#'
#' aoall <- aoall[do.call(order, aoall), ]
#' aoall$Value <- runif(nrow(aoall))
#' aoall_shk <- ems_custom_shock("aoall", aoall)
#' @export
ems_custom_shock <- function(var,
                             input
) {
if (missing(var)) {
  .cli_missing(var)
}
if (missing(input)) {
  .cli_missing(input)
}
args_list <- mget(names(formals()))
call <- match.call()
shock <- .implement_shock(
  args_list = args_list,
  class = "custom",
  call = call
)
shock
}