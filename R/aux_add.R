#' @importFrom purrr map_lgl list_flatten
#' @keywords internal
#' @noRd
.add_aux <- function(aux_input,
                     i_data,
                     data_call) {
  
  if (any(purrr::map_lgl(aux_input, is.list))) {
    aux_input <- purrr::list_flatten(aux_input)
  }

  if (!is.list(aux_input)) {
    aux_input <- list(aux_input)
  }

  aux_input <- aux_input[purrr::map_lgl(aux_input, \(a) {
    !class(a)[[1]] %in% .o_full_exclude()
  })]

  for (aux in seq_along(aux_input)) {
    input <- aux_input[[aux]]
    input_name <- class(input)[[1]]
    aux_call <- attr(input, "call")
    
    i_data <- .merge_aux(input = input,
                         input_name = input_name,
                         i_data = i_data,
                         aux_call = aux_call,
                         data_call = data_call)
  }

  return(i_data)
}