#' Write teems example models and scripts to file
#'
#' teems comes bundled with example models and scripts. This
#' function facilitates access.
#'
#' @param model Character vector of length 1, name of model.
#'   Options include:
#'  * `"GTAP-RE"`: GTAP Rational Expectations
#'  * `"GTAP-INT"`: GTAP Intertemporal
#'  * `"GTAPv7"`: Standard GTAP model
#'  * `"GTAPv6"`: Classic GTAP model
#' @param type Character vector of length 1, (default is
#'   `"model_files"`). Determines the example output. Options
#'   include:
#'  * `"model_files"`: Model file and closure file
#'  * `"scripts"`: Example scripts for the chosen `model`.
#'   `dat_input`, `par_input`, and `set_input` are required.
#' @param dat_input Character vector of length 1, (default is
#'   `NULL`, only used when `type = "scripts"`) file name in
#'   working directory or path to a data input containing
#'   basedata coefficient data (e.g., a HAR file).
#' @param par_input Character vector of length 1, (default is
#'   `NULL`, only used when `type = "scripts"`) file name in
#'   working directory or path to a data input containing
#'   parameter coefficient data (e.g., a HAR file).
#' @param set_input Character vector of length 1, (default is
#'   `NULL`, only used when `type = "scripts"`) file name in
#'   working directory or path to a data input containing set
#'   elements and attributes (e.g., a HAR file).
#' @param write_dir Character vector of length 1, (default is
#'   `tools::R_user_dir("teems")`). Directory where
#'   files will be written.
#' 
#' @importFrom tools R_user_dir
#'
#' @return For `type = "model_files"`: a named character vector with
#'   elements `model_file` and `closure_file`. For `type = "scripts"`: a
#'   character vector of paths to the written script files.
#' @examples
#' \dontrun{
#' # Write GTAP-RE model files to `tools::R_user_dir("teems")`
#' ems_example("GTAP-RE")
#' 
#' # Generate GTAP-RE example scripts
#' ems_example(model = "GTAP-RE",
#'             type = "scripts",
#'             dat_input = "v7data/gsdfdat.har",
#'             par_input = "v7data/gsdfpar.har",
#'             set_input = "v7data/gsdfset.har",
#'             write_dir = getwd())
#' }
#' @export
ems_example <- function(model,
                        type = c("model_files", "scripts"),
                        dat_input = NULL,
                        par_input = NULL,
                        set_input = NULL,
                        write_dir = tools::R_user_dir("teems")
) {
if (missing(model)) {
  .cli_missing(model)
}
args_list <- mget(names(formals()))
call <- match.call()
path <- .implement_exp(
  args_list = args_list,
  call = call
)
path
}