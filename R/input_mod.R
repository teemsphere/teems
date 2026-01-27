#' @keywords internal
#' @noRd
.mod_input <- function(input = NULL,
                       method_coeff = NULL,
                       args_list,
                       model,
                       call) {
  m_input <- list()
  method_coeff <- method_coeff[method_coeff %in% names(call)]
  
  if (!length(input) %=% 0L) {
    m_input <- c(m_input, input)
  }
  
  if (!length(method_coeff) %=% 0L) {
    m_input <- c(m_input, args_list[method_coeff])
    method_file <- deparse(call$model_input)
  }

  for (nme in names(m_input)) {
    input <- m_input[[nme]]
    attr(input, "name") <- nme
    if (nme %in% model$header) {
      class(input) <- c(class(input), "Read")
    } else if (nme %in% model$name && any(grepl(nme, model$comp1))) {
      class(input) <- c(class(input), "Formula")
      if (nme %in% method_coeff) {
        attr(input, "file") <- method_file
      }
    } else {
      .cli_action(
        model_err$invalid_input,
        action = "abort",
        call = call
      )
    }

    model <- .inject_value(
      input = input,
      nme = nme,
      model = model,
      call = call
    )
  }
  return(model)
}