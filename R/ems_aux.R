#' Load and prepare auxiliary data inputs
#'
#' @description `ems_aux()` loads and prepares data for input
#'   into the `ems_data()` function.
#'
#' @param input Character vector of length 1, file name in
#'   working directory or path to a data input containing
#'   basedata coefficient data. Currently supports a data frame
#'   or data frame extension (e.g., tibble, data table) for
#'   `"dat"` and `"par"` input `type`, a vector for `"set"` input
#'   `type`, or path to a CSV file or GTAP HAR files.
#'
#'   Where a header is to be replaced, the accepted set names
#'   (i.e., column names) within "input" must conform to the same
#'   format as the header-specific output from [`ems_data()`].
#'   Where a novel header is introduced, set names must conform
#'   to the hybrid format consisting of the concatenated set name
#'   and index within the coefficient declaration (e.g. REGr for
#'   the SAVE coefficient).
#' @param type Character vector of length 1, type of data that is
#'   loaded. One of the following:
#'  * `"dat"` inputs undergo simple sum aggregation.
#'  * `"par"` inputs undergo mean aggregation or weighted mean 
#'  aggregation.
#'  * `"set"` inputs represent final set elements.
#' @param header Character vector of length 1 (default is
#'   `NULL`). Must be provided if the input is not a GTAP HAR
#'   file. If provided and already present in other loaded data,
#'   will overwrite existing values. If not already present, will
#'   be appended to existing data.
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
#' # specify data type and header (in the case of single headers)
#' dat_input <- ems_aux(input = POP,
#'                      type = "dat",
#'                      header = "POP")
#'                      
#' par_input <- ems_aux(input = SUBP,
#'                      type = "par",
#'                      header = "SUBP")
#'                    
#' HAR_input <- ems_aux(input = aux_har_file,
#'                      type = "par")
#'          
#' # single auxiliary input          
#' .data <- ems_data(dat_input = "path/to/v6_data/v10A/flexagg10AY14/gsddat.har",
#'                   par_input = "path/to/v6_data/v10A/flexagg10AY14/gsdpar.har",
#'                   set_input = "path/to/v6_data/v10A/flexagg10AY14/gsdset.har",
#'                   aux_input = dat_input,
#'                   REG = "full",
#'                   TRAD_COMM = "full",
#'                   ENDW_COMM = "labor_agg")
#'                   
#' # multiple auxiliary inputs          
#' .data <- ems_data(dat_input = "path/to/v6_data/v10A/flexagg10AY14/gsddat.har",
#'                   par_input = "path/to/v6_data/v10A/flexagg10AY14/gsdpar.har",
#'                   set_input = "path/to/v6_data/v10A/flexagg10AY14/gsdset.har",
#'                   aux_input = list(dat_input, par_input, HAR_input),
#'                   REG = "full",
#'                   TRAD_COMM = "full",
#'                   ENDW_COMM = "labor_agg")
#' }
#' @return An object ready for input into the `"aux_input"`
#'   argument of [`ems_data()`]
#' @export
ems_aux <- function(input,
                    type,
                    header = NULL)
{
if (missing(input)) {
  .cli_missing(input)
}
if (missing(type)) {
  .cli_missing(type)
}
args_list <- mget(names(formals()))
call <- match.call()
aux <- .implement_aux(
  args_list = args_list,
  call = call
)
aux
}