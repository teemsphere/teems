#' @keywords internal
#' @noRd
.check_tab_file <- function(tab_file,
                            call) {
  tab <- readChar(
    tab_file,
    file.info(tab_file)[["size"]],
    useBytes = TRUE
  )
  tab <- .tab_to_utf8(tab)

  statements <- .check_statements(
    tab = tab,
    call = call
  )

  return(statements)
}

#' @keywords internal
#' @noRd
.tab_to_utf8 <- function(tab) {
  tab <- sub("^\xEF\xBB\xBF", "", tab, useBytes = TRUE)
  if (!validUTF8(tab)) {
    tab <- iconv(tab, from = "latin1", to = "UTF-8", sub = "")
  }
  Encoding(tab) <- "UTF-8"
  return(tab)
}
