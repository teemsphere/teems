#' Write teems example models and scripts to file
#'
#' teems comes bundled with example models and scripts. This
#' function facilitates access.
#'
#' @inheritParams ems_data
#' @inheritParams ems_deploy
#'
#' @param model Character vector of length 1, (default is
#'   `NULL`). Name of model. Options include:
#'  * `"GTAP-RE"`: GTAP Rational Expectations
#'  * `"GTAP-INT"`: GTAP Intertemporal
#'  * `"GTAPv7"`: Standard GTAP model
#'  * `"GTAPv6"`: Classic GTAP model
#' @param write_dir Character vector of length 1, (default is
#'   `tools::R_user_dir("teems", "cache")`). Directory where
#'   files will be written.
#' @param type Character vector of length 1. Determines the
#'   example output. Options include:
#'  * `"model"`: Model file and closure file
#'  * `"scripts"`: Example scripts for the chosen `model`.
#'   `dat_input`, `par_input`, and `set_input` are required.
#'   `target_format` must be specified if a data format
#'   conversion is to take place.
#'
#' @importFrom tools R_user_dir
#'
#' @examples
#' # write GTAP-RE model files to `tools::R_user_dir("teems", "cache")`
#' ems_example("GTAP-RE")
#' @return For `type = "model_files"`: a named list with elements `model_file`
#'   and `closure_file`. For `type = "scripts"`: a character vector of paths to
#'   the written script files.
#' @export
ems_example <- function(model,
                        write_dir = tools::R_user_dir("teems", "cache"),
                        type = c("model_files", "scripts"),
                        dat_input = NULL,
                        par_input = NULL,
                        set_input = NULL,
                        target_format = NULL)
{
if (missing(model)) {
  .cli_missing(model)
}
args_list <- mget(names(formals()))
call <- match.call()
path <- .implement_exp(args_list = args_list,
                       call = call)
path
}