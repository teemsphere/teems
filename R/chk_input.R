#' @importFrom tools file_ext
#'
#' @keywords internal
#' @noRd
.check_input <- function(file = NULL,
                         valid_ext,
                         call) {

  file_ext <- tolower(tools::file_ext(basename(file)))

  file <- .check_file(
    file = file,
    valid_ext = valid_ext,
    call = call,
    file_ext = file_ext
  )

  return(file)
}