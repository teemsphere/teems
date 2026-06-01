#' @title Convert GTAP HAR files and data formats
#' @export
#' @description `GTAP_convert()` converts GTAP HAR database files
#'   into lists of arrays as well as converts between the classic
#'   v6.2 and standard v7.0 data formats. The output can then be
#'   passed directly to [`ems_data()`] via its `dat_input`,
#'   `par_input`, and `set_input` arguments, or written to disk
#'   for later use.
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
#'   `NULL`). The target format to convert to. Currently supports
#'   `"GTAPv6"` or `"GTAPv7"`. If `NULL`, no data format
#'   conversion will take place.
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
#'   dat_har = "path/to/gsdfdat.har",
#'   par_har = "path/to/gsdfpar.har",
#'   set_har = "path/to/gsdfset.har",
#'   target    = "GTAPv6"
#' )
#'
#' # Convert v6.2 files to v7.0 format
#' converted2v7 <- GTAP_convert(
#'   dat_har = "path/to/gsddat.har",
#'   par_har = "path/to/gsdpar.har",
#'   set_har = "path/to/gsdset.har",
#'   target    = "GTAPv7"
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