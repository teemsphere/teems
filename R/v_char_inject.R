#' @importFrom data.table fread
#' 
#' @keywords internal
#' @noRd
.inject_v_char <- function(input,
                           model,
                           call) {
  nme <- attr(input, "name")
  tail_class <- sub(class(input)[2], "", class(input)[1], fixed = TRUE)
  input <- .check_input(
    file = input,
    valid_ext = "csv",
    call = call
  )

  input <- data.table::fread(input)
  attr(input, "name") <- nme
  head_class <- class(input)[length(class(input))]
  class(input) <- c(paste0(head_class, tail_class), class(input))
  model <- .inject_value(
    input = input,
    model = model,
    call = call
  )
  return(model)
}