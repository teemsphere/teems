#' @keywords internal
#' @noRd
.shock_preload <- function(input,
                           type,
                           call) {
  UseMethod(".shock_preload")
}

#' @importFrom data.table fread
#' 
#' @keywords internal
#' @noRd
#' @method .shock_preload character
#' @export
.shock_preload.character <- function(input,
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

#' @importFrom data.table is.data.table as.data.table
#' 
#' @keywords internal
#' @noRd
#' @method .shock_preload data.frame
#' @export
.shock_preload.data.frame <- function(input,
                                      type,
                                      call) {

  if (!data.table::is.data.table(input)) {
    input <- data.table::as.data.table(input)
  }
  
  .chk_shk_col(input = input,
               type = type,
               call = call)
  
  input$Value <- as.numeric(input$Value)
  return(input)
}