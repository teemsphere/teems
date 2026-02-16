#' Get path to teems example models and scripts
#'
#' teems comes bundled with some example files in its
#' `inst/extdata`, `inst/models`, and `inst/scripts` directories.
#' This function make them easy to access.
#' 
#' @param file Name of file. If `NULL`, the example files will be
#'   listed.
#' @param dir Directory path within /inst containing `"file"`
#'
#' @details Function modified from `readxl::readxl_example()`
#'
#' @importFrom tools R_user_dir
#' @export
#' @examples
#' ems_example("GTAP_RE.tab")
ems_example <- function(file,
                        dir = "extdata")
{
temp_dir <- tools::R_user_dir(package = "teems", which = "cache")
if (!dir.exists(temp_dir)) {
  dir.create(temp_dir)
}

origin <- system.file(file.path(dir, file), package = "teems", mustWork = TRUE)
file.copy(
  from = origin,
  to = temp_dir
)

file_path <- file.path(temp_dir, file)
file_path
}