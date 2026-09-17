#' @title Load GTAP HAR files and convert data formats
#' @export
#' @description `GTAP_convert()` reads a GTAP database's HAR files
#'   into lists of arrays at full resolution, without any
#'   aggregation. With `target` it can also convert a standard GTAP
#'   database between the classic v6.2 and standard v7 data formats,
#'   or prepare a GTAP-AEZ, GTAP-E or GTAP-Power database for its
#'   model. The output can be inspected or modified in R, passed
#'   directly to [`ems_data()`] via its `dat_input`, `par_input`, and
#'   `set_input` arguments, or written to disk for later use.
#' @return A named list with elements `dat`, `par`, and `set`,
#'   each containing a list of arrays corresponding to database
#'   headers. Suitable for direct use with [`ems_data()`].
#' @param dat_har Character vector of length 1, file name in
#'   working directory or path to a GTAP HAR file with basedata
#'   coefficient data.
#' @param par_har Character vector of length 1, file name in
#'   working directory or path to a GTAP HAR file with parameter
#'   coefficient data.
#' @param set_har Character vector of length 1, file name in
#'   working directory or path to a GTAP HAR file with set
#'   elements and attributes.
#' @param target Character vector of length 1 (default is
#'   `NULL`). One of:
#'   * `NULL`: the files are read as they are, with no conversion
#'   or preparation.
#'   * `"GTAPv6"` or `"GTAPv7"`: converts a standard GTAP database to
#'   the v6.2 or v7 data format. Version conversion applies to the
#'   standard GTAP database only.
#'   * `"GTAP-AEZ"`, `"GTAP-E"` or `"GTAP-EP"`: prepares a v7-format
#'   GTAP-AEZ, GTAP-E (12a) or GTAP-Power (12a) database for the
#'   corresponding model, adding the sets and parameters the model
#'   reads that the database does not ship. These targets do not
#'   convert between data formats: a v6-format database (the GTAP
#'   10a layers) is refused, as are the GTAP 11c E and Power
#'   releases. [`ems_data()`] applies the same preparation on its
#'   own when it detects one of these databases.
#' @seealso [`ems_data()`] for loading and preparing converted
#'   data for a model run.
#' @examples
#' \dontrun{
#' # The following examples require input data. See
#' # https://teemsphere.github.io/ to get started.
#'
#' # Convert HAR files to lists of arrays
#' ls_arrays <- GTAP_convert(
#'   dat_har = "path/to/gsdfdat.har",
#'   par_har = "path/to/gsdfpar.har",
#'   set_har = "path/to/gsdfset.har"
#' )
#'
#' # Convert v7.0 files to v6.2 format
#' converted2v6 <- GTAP_convert(
#'   dat_har = "v7_data/gsdfdat.har",
#'   par_har = "v7_data/gsdfpar.har",
#'   set_har = "v7_data/gsdfset.har",
#'   target = "GTAPv6"
#' )
#'
#' # Convert v6.2 files to v7.0 format
#' converted2v7 <- GTAP_convert(
#'   dat_har = "v6_data/gsddat.har",
#'   par_har = "v6_data/gsdpar.har",
#'   set_har = "v6_data/gsdset.har",
#'   target = "GTAPv7"
#' )
#' }
GTAP_convert <- function(dat_har,
                         par_har,
                         set_har,
                         target = NULL
) {
if (missing(dat_har)) {
  .cli_missing(dat_har)
}
if (missing(par_har)) {
  .cli_missing(par_har)
}
if (missing(set_har)) {
  .cli_missing(set_har)
}
args_list <- mget(names(formals()))
call <- match.call()
.data <- .implement_convert(
  args_list = args_list,
  call = call
)
.data
}