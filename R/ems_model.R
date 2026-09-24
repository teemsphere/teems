#' @title Parse and modify model and closure files
#' @export
#' @description Parses the model and closure files, conducts
#'   pre-deployment checks, and carries out specified
#'   modifications.
#' @return A tibble comprising parsed model statements to be
#'   passed to the `model` argument of [`ems_deploy()`].
#' @param model_file Character vector length 1, path to a TABLO
#'   file with .tab extension. See
#'   \href{https://github.com/teemsphere/teems-models}{teems-models}
#'   for vetted models and compatible Tablo file formatting.
#' @param closure_file Character vector length 1, path to a
#'   closure file with .cls extension. See
#'   \href{https://github.com/teemsphere/teems-models}{teems-models}
#'   for closure file formatting.
#' @param backsolve Character vector of variable length (default
#'   is `NULL`), variable names to substitute out of the system
#'   by backsolving. Unnamed entries resolve their defining
#'   equation by the `E_<variable>` convention; use named entries
#'   for exceptions (e.g., `c(qgd = "E_qgdtot")`). Backsolved
#'   variables must be endogenous; their values remain available
#'   in solve outputs. Nominations are validated against the
#'   GEMPACK substitution requirements (GEMPACK manual, section
#'   14.1.10).
#' @param ignore_condense Logical length 1 (default is `FALSE`).
#'   If `TRUE`, `Substitute` and `Backsolve` statements found in the
#'   model file are ignored (the equivalent of the GEMPACK `ICT`
#'   option). In-TAB `Substitute` statements are always executed as
#'   backsolves. In-TAB `Omit` statements hold the omitted variables
#'   exogenous and unshocked, which is what omission means in
#'   GEMPACK; the variables stay in the model and in the outputs.
#' @param ... A named pairlist assigning values to model
#'   coefficients. Name must match a coefficient declared in the
#'   model file. Value may be a length-1 numeric, a data frame or
#'   data frame extension (e.g., tibble, data.table), or a path
#'   to a CSV file.
#' @details If a length-1 numeric is supplied via `...`, this
#'   value is applied uniformly across all tuples for that
#'   coefficient. For heterogeneous values, data frame and CSV
#'   inputs must contain a `Value` column and one column per set
#'   index using the model-specific naming convention (set name
#'   concatenated with its index letter, e.g., `REGr`, `COMMc`).
#'   Inputs are subject to structure checks against the set
#'   aggregations specified in [`ems_data()`] and all tuples must
#'   be present.
#' @seealso [`ems_example()`] for writing teems example models and scripts to file. [`ems_deploy()`] for loading the output of this
#'   function as well as conducting closure swaps.
#' @examples
#' # Simple static model retrieval and load
#' GTAPv7 <- ems_example("GTAPv7", tempdir())
#' ems_model(GTAPv7[["model_file"]], GTAPv7[["closure_file"]])
#'
#' # Retrieve intertemporal model
#' GTAP_RE <- ems_example("GTAP-RE", tempdir())
#' \donttest{
#' # Construct data frame
#' sectors <- c("crops", "food", "livestock", "mnfcs", "svces")
#' regions <- c("usa", "chn", "row")
#' time_steps <- 0:5
#'
#' SUBPAR <- expand.grid(COMMc = sectors,
#'                       REGr = regions,
#'                       ALLTIMEt = time_steps)
#' SUBPAR$Value <- runif(nrow(SUBPAR))
#'
#' # Model load with:
#' # 1) uniform numeric value applied to KAPPA coefficient
#' # 2) heterogeneous values allocated to SUBPAR via data frame
#'
#' ems_model(model_file = GTAP_RE[["model_file"]],
#'           closure_file = GTAP_RE[["closure_file"]],
#'           KAPPA = 0.03,
#'           SUBPAR = SUBPAR)
#' }
ems_model <- function(
    model_file,
    closure_file,
    backsolve = NULL,
    ignore_condense = FALSE,
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