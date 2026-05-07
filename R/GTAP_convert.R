#' Convert data between GTAP v6.2 and v7.0 formats
#'
#' @description `GTAP_convert()` converts GTAP HAR database files
#'   between the classic v6.2 and standard v7.0 data formats. The
#'   converted output can be passed directly to [`ems_data()`]
#'   via its `dat_input`, `par_input`, and `set_input` arguments,
#'   or written to disk for later use.
#'
#' @inheritParams ems_data
#'
#' @param origin Character vector of length 1. The format of the
#'   input HAR files. One of `"GTAPv6"` or `"GTAPv7"`.
#' @param target Character vector of length 1. The target format
#'   to convert to. One of `"GTAPv6"` or `"GTAPv7"`. Must differ
#'   from `origin`.
#'
#' @return A named list with elements `dat`, `par`, and `set`,
#'   each containing the converted data tables in the target
#'   format. Suitable for direct use with [`ems_data()`].
#'
#' @seealso [`ems_data()`] for loading and preparing converted
#'   data for a model run.
#'
#' @examples
#' \dontrun{
#' # Convert v7.0 files to v6.2 format
#' converted <- GTAP_convert(
#'   dat_input = "path/to/gsdfdat.har",
#'   par_input = "path/to/gsdfpar.har",
#'   set_input = "path/to/gsdfset.har",
#'   origin    = "GTAPv7",
#'   target    = "GTAPv6"
#' )
#'
#' # Convert v6.2 files to v7.0 format
#' converted <- GTAP_convert(
#'   dat_input = "path/to/gsddat.har",
#'   par_input = "path/to/gsdpar.har",
#'   set_input = "path/to/gsdset.har",
#'   origin    = "GTAPv6",
#'   target    = "GTAPv7"
#' )
#' }
#'
#' @export
GTAP_convert <- function(dat_input,
                         par_input,
                         set_input,
                         origin,
                         target
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