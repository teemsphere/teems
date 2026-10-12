#' @keywords internal
#' @noRd
.shk_preload <- function(input,
                         type,
                         call) {
  UseMethod(".shk_preload")
}

#' @importFrom data.table fread
#' @keywords internal
#' @noRd
#' @method .shk_preload character
#' @export
.shk_preload.character <- function(input,
                                     type,
                                     call) {

  input <- .check_input(
    file = input,
    valid_ext = "csv",
    call = call
  )
  
  input <- data.table::fread(input)
  .chk_shk_col(input = input,
               type = type,
               call = call)
  input$Value <- as.numeric(input$Value)
  return(input)
}

#' @importFrom data.table is.data.table as.data.table copy set
#' @keywords internal
#' @noRd
#' @method .shk_preload data.frame
#' @export
.shk_preload.data.frame <- function(input,
                                      type,
                                      call) {

  if (!data.table::is.data.table(input)) {
    input <- data.table::as.data.table(input)
  } else {
    input <- data.table::copy(input)
  }

  for (col in colnames(input)) {
    if (is.factor(input[[col]])) {
      data.table::set(input, j = col, value = as.character(input[[col]]))
    }
  }

  .chk_shk_col(input = input,
               type = type,
               call = call)
  
  input$Value <- as.numeric(input$Value)
  return(input)
}
