#' Parse and modify model and closure files
#'
#' Parses the Tablo model file and closure, conducts
#' pre-deployment checks, and determines temporal dynamics.
#'
#' @param model_file Path to a `.tab` Tablo file. See
#'   \href{https://github.com/teemsphere/teems-models}{teems-models}
#'   for vetted models and compatible Tablo file formatting.
#' @param closure_file Path to a `.cls` closure file. See
#'   \href{https://github.com/teemsphere/teems-models}{teems-models}
#'   for closure file formatting.
#' @param var_omit Character vector of variable names to
#'   substitute with `0` in the model and remove from the closure
#'   (default `NULL`).
#' @param ... A named pairlist assigning values to model
#'   coefficients. Each name must match a coefficient declared in
#'   the model file. Each value may be a length-1 numeric, a data
#'   frame or data frame extension (e.g., tibble, data.table), or
#'   a path to a CSV file.
#'
#' @details
#'   A length-1 numeric supplied via `...` is applied uniformly
#'   across all set combinations for that coefficient. Data frame
#'   and CSV inputs must contain a `Value` column and one column
#'   per set index using the model-specific naming convention (set
#'   name concatenated with its index letter, e.g., `REGr`,
#'   `COMMc`). Inputs are subject to structure checks against the
#'   set aggregations specified in [`ems_data()`].
#'
#' @return A tibble passed to the `model` argument of
#'   [`ems_deploy()`].
#'
#' @seealso [`ems_example()`] for loading example models and
#'   scripts. [`ems_deploy()`] for loading the output of this
#'   function as well as conducting any closure swaps.
#' 
#' @examples
#' # simple model load
#' GTAPv7 <- ems_example("GTAPv7")
#' model <- ems_model(GTAPv7[["model_file"]],
#'                    GTAPv7[["closure_file"]])
#' 
#' # model load with variable omission
#' # uniform numeric value applied to KAPPA coefficient
#' # heterogeneous values allocated to SUBPAR via data frame
#' GTAP_RE <- ems_example("GTAP-RE")
#' sectors <- c("crops", "food", "livestock", "mnfcs", "svces")
#' regions <- c("usa", "chn", "row")
#' time_steps <- 0:5
#' 
#' SUBPAR <- expand.grid(COMMc = sectors,
#'                       REGr = regions,
#'                       ALLTIMEt = time_steps)
#' SUBPAR$Value <- runif(nrow(SUBPAR))
#' model <- ems_model(model_file = GTAP_RE[["model_file"]],
#'                    closure_file = GTAP_RE[["closure_file"]],
#'                    var_omit = c("atall", "avaall", "tfe", "tfm", "tgd", "tgm", "tid", "tim"),
#'                    KAPPA = 0.03,
#'                    SUBPAR = SUBPAR)
#' @export
ems_model <- function(
    model_file,
    closure_file,
    var_omit = NULL,
    ...
) {
if (missing(model_file)) {
  .cli_missing(model_file)
}
if (missing(closure_file)) {
  .cli_missing(closure_file)
}
args_list <- mget(names(formals()))
args_list$mod_coeff <- list(...)
call <- match.call()
model <- .implement_model(
  args_list = args_list,
  call = call
)
model
}