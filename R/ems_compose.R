#' Compose model results
#'
#' @description `ems_compose()` retrieves and processes results
#'   from a solved model run. Results are parsed according to the
#'   specified type (variables, coefficients, or base data). Data
#'   validation and consistency checks are performed during the
#'   parsing process.
#'
#' @inheritParams ems_solve
#'
#' @param type Character length 1, type of data to parse
#'   (default includes all). Choices:
#'   * `"all"`: All model variables and coefficients
#'   * `"variable"`: Percentage change values for model variables
#'   * `"coefficient"`: Values for model coefficients
#' @param name Character vector, a subset of the selected type
#'   filtered by name.
#' @param minimal Logical length 1, default `FALSE`. Whether to
#'   run a minimal number of checks and modifications to output
#'   data. If `TRUE` chronological data will not be loaded and
#'   the model file will not be parsed, meaning: 1) no post model
#'   set checks are conducted between model sets as written out
#'   from the model input and solver set binaries; and 2) time
#'   steps will remain in their model format. If a model input
#'   with no set writeout is used (e.g., [`solve_in_situ()`]),
#'   this option must be set to `TRUE`.
#' 
#' @importFrom rlang arg_match
#'
#' @seealso [`ems_solve()`] for running the model simulation.
#'
#' @examples
#' \dontrun{
#' inputdata <- ems_compose(cmf_path = cmf_path, type = "inputdata")
#' variables <- ems_compose(cmf_path = cmf_path, type = "variable")
#' coefficients <- ems_compose(cmf_path = cmf_path, type = "coefficient")
#' sets <- ems_compose(cmf_path = cmf_path, type = "set")
#' 
#' qfd <- ems_compose(cmf_path = cmf_path, type = "variable", name = "qfd")
#' }
#'
#' @return A list containing the parsed model results according
#'   to the specified type.
#' @export
ems_compose <- function(cmf_path,
                        type = c("all", "variable", "coefficient"),
                        name = NULL,
                        minimal = FALSE)
{
if (missing(cmf_path)) {
  .cli_missing(cmf_path)
}
args_list <- mget(names(formals()))
call <- match.call()
args_list$type <- rlang::arg_match(arg = type)
output <- .implement_compose(args_list = args_list,
                             call = call)
output
}