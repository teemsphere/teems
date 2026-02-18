#' Load and prepare data inputs
#'
#' @description `ems_data()` loads and prepares data for use
#'   within a CGE model run.
#'
#' @param dat_input Character vector of length 1, file name in
#'   working directory or path to a data input containing
#'   basedata coefficient data. Currently supports GTAP "dat" HAR
#'   files.
#' @param par_input Character vector of length 1, file name in
#'   working directory or path to a data input containing
#'   parameter coefficient data. Currently supports GTAP "par"
#'   HAR files.
#' @param set_input Character vector of length 1, file name in
#'   working directory or path to a data input containing set
#'   elements and attributes. Currently supports GTAP "set" HAR
#'   files.
#' @param time_steps Integer vector of variable length with
#'   initial value 0 or reference year corresponding to database
#' employed (default is `NULL`). Input format can be either the
#' chronological years of steps including initial reference year
#' or steps from t0.
#' @param aux_input List created with [`ems_aux()`], (default is
#'   `NULL`). Auxiliary data to be loaded either to replace
#'   existing headers or as additional headers.
#' @param target_format Character vector of length 1 (default is
#'   `NULL`). If not `NULL`, data will be converted to the target
#'   format. Currently supports "GTAPv6" and "GTAPv7".
#' @param ... A named pairlist assigning set mappings to model
#'   sets that are read in (not constructed via any set operation
#'   defined in the model file). Set mappings can be selected
#'   from a range of internally available options or provided by
#'   the user.
#'   * Internal mapping: character vector of length 1 corresponding to an
#'   internally available mapping
#'   * User-provided mapping: Character vector of length 1, file
#'   name in working directory or path to a .csv file.
#'
#'   See
#'   \href{https://github.com/teemsphere/teems-mappings}{teems-mappings}
#'   for internally available mappings and formatting for
#'   user-provided mappings.
#' @details
#' Missing section explaining the aggregation process
#' 
#' @return A list of data tables ready for input into the
#'   `"data"` argument of [`ems_deploy()`]
#'
#' @seealso [`ems_aux()`] preparing auxiliary input data.
#' @seealso [`ems_deploy()`] for using the output of this
#'   function.
#' 
#' @examples
#' \dontrun{
#' # Static models (no data conversion)
#' v6_data <- ems_data(dat_input = "path/to/v6_data/v10A/flexagg10AY14/gsddat.har",
#'                     par_input = "path/to/v6_data/v10A/flexagg10AY14/gsdpar.har",
#'                     set_input = "path/to/v6_data/v10A/flexagg10AY14/gsdset.har",
#'                     REG = "big3",
#'                     TRAD_COMM = "macro_sector",
#'                     ENDW_COMM = "labor_agg")
#'
#' v7_data <- ems_data(dat_input = "path/to/v7_data/v11c/flexAgg11c17/gsdfdat.har",
#'                     par_input = "path/to/v7_data/v11c/flexAgg11c17/gsdfpar.har",
#'                     set_input = "path/to/v7_data/v11c/flexAgg11c17/gsdfset.har",
#'                     REG = "AR5",
#'                     COMM = "agriculture",
#'                     ACTS = "agriculture",
#'                     ENDW = "labor_diff")
#'                      
#' # Data format conversion with user-provided REG mapping
#' v6_data <- ems_data(dat_input = "path/to/v7_data/v11c/flexAgg11c17/gsdfdat.har",
#'                     par_input = "path/to/v7_data/v11c/flexAgg11c17/gsdfpar.har",
#'                     set_input = "path/to/v7_data/v11c/flexAgg11c17/gsdfset.har",
#'                     REG = "path/to/user/mapping/WB7.csv",
#'                     TRAD_COMM = "manufacturing",
#'                     ENDW_COMM = "full",
#'                     target_format = "GTAPv6")
#'
#' # Intertemporal model (explicit time steps)
#' int_data <- ems_data(dat_input = "path/to/v6_data/v10A/flexagg10AY14/gsddat.har",
#'                      par_input = "path/to/v6_data/v10A/flexagg10AY14/gsdpar.har",
#'                      set_input = "path/to/v6_data/v10A/flexagg10AY14/gsdset.har",
#'                      REG = "WB23",
#'                      TRAD_COMM = "services",
#'                      ENDW_COMM = "labor_agg",
#'                      time_steps = c(0, 1, 2, 3, 4, 6, 8, 10, 12, 14, 16))
#'  
#' # Intertemporal model (chronological time steps)
#' int_data <- ems_data(dat_input = "path/to/v7_data/v11c/flexAgg11c17/gsdfdat.har",
#'                      par_input = "path/to/v7_data/v11c/flexAgg11c17/gsdfpar.har",
#'                      set_input = "path/to/v7_data/v11c/flexAgg11c17/gsdfset.har",
#'                      REG = "R32",
#'                      COMM = "medium",
#'                      ACTS = "medium",
#'                      ENDW = "labor_diff",
#'                      time_steps = c(2017, 2018, 2020, 2022, 2024, 2026, 2028, 2030))                       
#' }           
#' @export
ems_data <- function(dat_input,
                     par_input,
                     set_input,
                     time_steps = NULL,
                     aux_input = NULL,
                     target_format = NULL,
                     ...
) {
if (missing(dat_input)) {
  .cli_missing(dat_input)
}
if (missing(par_input)) {
  .cli_missing(par_input)
}
if (missing(set_input)) {
  .cli_missing(set_input)
}
args_list <- mget(names(formals()))
args_list$set_mappings <- list(...)
call <- match.call()
.data <- .implement_data(args_list = args_list,
                         call = call)
.data
}