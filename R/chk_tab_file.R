#' @keywords internal
#' @noRd
.check_tab_file <- function(tab_file,
                            call) {
  tab <- readChar(
    tab_file,
    file.info(tab_file)[["size"]]
  )

  statements <- .check_statements(
    tab = tab,
    call = call
  )

  return(statements)
}