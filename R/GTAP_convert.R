#' @title Convert data between GTAP v6.2 and v7.0 formats
#' @export
#' @description `GTAP_convert()` converts GTAP HAR database files
#'   between the classic v6.2 and standard v7.0 data formats. The
#'   converted output can then be passed directly to
#'   [`ems_data()`] via its `dat_input`, `par_input`, and
#'   `set_input` arguments, or written to disk for later use.
#' @return A named list with elements `dat`, `par`, and `set`,
#'   each containing the converted data tables in the target
#'   format. Suitable for direct use with [`ems_data()`].
#' @param dat_har Character vector of length 1, file name in
#'   working directory or path to a GTAP HAR file with basedata
#'   coefficient data.
#' @param par_har Character vector of length 1, file name in
#'   working directory or path to a GTAP HAR file with parameter
#'   coefficient data.
#' @param set_har Character vector of length 1, file name in
#'   working directory or path to a GTAP HAR file with set
#'   elements and attributes.
#' @param origin Character vector of length 1. The format of the
#'   input HAR files. One of `"GTAPv6"` or `"GTAPv7"`. Must
#'   differ from `target`.
#' @param target Character vector of length 1. The target format
#'   to convert to. One of `"GTAPv6"` or `"GTAPv7"`. Must differ
#'   from `origin`.
#' @seealso [`ems_data()`] for loading and preparing converted
#'   data for a model run.
#' @examples
#' \dontrun{
#' # The following examples require input data. See
#' # https://teemsphere.github.io/ to get started.
#'
#' # Convert v7.0 files to v6.2 format
#' converted <- GTAP_convert(
#'   dat_har = "path/to/gsdfdat.har",
#'   par_har = "path/to/gsdfpar.har",
#'   set_har = "path/to/gsdfset.har",
#'   origin    = "GTAPv7",
#'   target    = "GTAPv6"
#' )
#'
#' # Convert v6.2 files to v7.0 format
#' converted <- GTAP_convert(
#'   dat_har = "path/to/gsddat.har",
#'   par_har = "path/to/gsdpar.har",
#'   set_har = "path/to/gsdset.har",
#'   origin    = "GTAPv6",
#'   target    = "GTAPv7"
#' )
#' }
GTAP_convert <- function(dat_har,
                         par_har,
                         set_har,
                         origin,
                         target
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
if (missing(origin)) {
  .cli_missing(origin)
}
if (missing(target)) {
  .cli_missing(target)
}
args_list <- mget(names(formals()))
call <- match.call()
.data <- .implement_convert(
  args_list = args_list,
  call = call
)
.data
}