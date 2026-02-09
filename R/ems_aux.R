#' Load and prepare auxiliary data inputs
#'
#' @description `ems_aux()` loads and prepares data for input
#'   into the `ems_data()` function.
#'
#' @param dat Auxiliary inputs for non-parameter coefficients
#'   (default `NULL`).
#' @param par Auxiliary inputs for parameters coefficients
#'   (default `NULL`).
#' @param set Auxiliary inputs for sets (default `NULL`).
#'
#' @section Input structure: CSV inputs must contain all tuples
#'   associated with an unaggregated header. These will be
#'   subject to checks and aggregated according to data type and set mappings 
#'   provided in [`ems_data()`].
#' * `"dat"` Inputs undergo simple sum aggregation.
#' * `"par"` Inputs undergo mean aggregation.
#' * `"set"` Inputs are subsetted.
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
#' # example code
#' 
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