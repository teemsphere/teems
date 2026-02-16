#' Load and prepare auxiliary data inputs
#'
#' @description `ems_aux()` loads and prepares data for input
#'   into the `ems_data()` function.
#'
#' @param dat Auxiliary inputs for non-parameter coefficients
#'   (default `NULL`).
#' @param par Auxiliary inputs for parameters coefficients
#'   (default `NULL`).
#' @param set Under development. Auxiliary inputs for sets (default `NULL`).
#'
#' @section Input structure: CSV inputs must contain all tuples
#'   associated with an unaggregated header. These will be
#'   subject to checks and aggregated according to data type and
#'   set mappings provided in [`ems_data()`].
#' * `"dat"` inputs undergo simple sum aggregation.
#' * `"par"` inputs undergo mean aggregation or weighted mean aggregation.
#' * `"set"` Under development. Inputs will be subsetted.
#' @section Input Format: Each parameter (`dat`, `par`, `set`)
#'   accepts a list with list elements in one of the following
#'   formats:
#' * **Data frame**: A named list where the name corresponds to
#'   the header and value is a data frame or extension (tibble,
#'   data.table).
#' * **CSV file**: A named list where the name corresponds to
#'   the header and value is a file path to a CSV.
#' * **HAR file**: An unnamed character vector with the path to
#'   a GTAP header array file.
#'
#' @return A list object ready for input into the `"aux_input"`
#'   argument of [`ems_data()`]
#'
#' @seealso [`ems_data()`] for using the output of this function.
#' @examples
#' \dontrun{
#' # unaggregated REG and TRAD_COMM sets
#' .data <- ems_data(dat_input = "path/to/v6_data/v10A/flexagg10AY14/gsddat.har",
#'                   par_input = "path/to/v6_data/v10A/flexagg10AY14/gsdpar.har",
#'                   set_input = "path/to/v6_data/v10A/flexagg10AY14/gsdset.har",
#'                   REG = "full",
#'                   TRAD_COMM = "full",
#'                   ENDW_COMM = "labor_agg")
#' 
#' POP <- .data$POP
#' SUBP <- .data$SUBP
#' 
#' # new values
#' POP$Value <- POP$Value * 1.5
#' SUBP$Value <- runif(nrow(SUBP))
#' 
#' # an auxiliary HAR file
#' aux_har_file <- "/path/to/auxiliary_har/GDYNflexagg10A_Y14/gdpextra.har"
#' 
#' # load according to data type
#' aux_input <- ems_aux(dat = list(POP = POP),
#'                      par = list(SUBP = SUBP, "/home/mpc/dat/GDYN/GDYNflexagg10A_Y14/gdpextra.har"))
#' }
#' @export
ems_aux <- function(dat = NULL,
                    par = NULL,
                    set = NULL)
{
args_list <- mget(names(formals()))
call <- match.call()
aux <- .implement_aux(args_list = args_list,
                      call = call)
aux
}