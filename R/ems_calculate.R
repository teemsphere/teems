#' @title Calculate a model file's coefficients without a simulation
#' @export
#' @description Runs a Tablo file's `Read`, `Formula` and
#'   `Assertion` statements on the input files given and returns
#'   the coefficients as read and calculated, without a closure,
#'   shocks or a solve (GEMPACK's data programs and
#'   `simulation = no`, manual 5.1.2 and 25.1.8). Any Tablo file
#'   can be calculated: one without `Equation` statements (data
#'   preparation, conversion or checking programs) as well as a
#'   full model, whose equations are then left unsolved.
#'   `PostSim` sections are not run, since they need a simulation,
#'   and their coefficients are not returned.
#'   A failed `Assertion` stops the calculation.
#' @return A tibble of coefficients, as returned by
#'   [`ems_compose()`].
#' @param model_file Character vector length 1, path to a Tablo
#'   file with .tab extension.
#' @param ... Named arguments corresponding to input files. Names
#'   must correspond to "File" statements within `model_file`;
#'   values are file paths. Every input file the model file reads
#'   is required. Files are used as given, without checks or
#'   modifications.
#' @param model_dir Character vector length 1, directory where the
#'   input files are copied (if not all already there) and outputs
#'   are written. Defaults to the `tempdir` option.
#' @examples
#' \dontrun{
#' ems_calculate(
#'   model_file = "path/to/gtapview.tab",
#'   GTAPSETS = "path/to/sets.har",
#'   GTAPDATA = "path/to/basedata.har"
#' )
#' }
ems_calculate <- function(model_file,
                          ...,
                          model_dir = NULL) {
if (missing(model_file)) {
  .cli_missing(model_file)
}
call <- match.call()
if (missing(...)) {
  .cli_action(solve_err$no_insitu_inputs,
    action = "abort",
    call = call
  )
}
output <- .implement_calculate(
  model_file = model_file,
  input_files = list(...),
  model_dir = model_dir %|||% .o_tempdir(),
  call = call
)
output
}
