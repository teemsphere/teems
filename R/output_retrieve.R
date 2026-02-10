#' @importFrom purrr pluck
#' 
#' @keywords internal
#' @noRd
.retrieve_output <- function(type,
                             comp_extract,
                             name,
                             paths,
                             parsed,
                             sets,
                             time_steps,
                             minimal,
                             call) {

  compose_variable <- type %in% c("variable", "all")
  compose_coefficient <- type %in% c("coefficient", "all")
  output <- list()

  if (compose_variable) {
    var_union <- parsed$var_union

    output$variable <- .compose_var(
      xc = parsed$xc,
      var_extract = comp_extract$variable,
      vars = var_union,
      sets = sets,
      time_steps = time_steps,
      minimal = minimal,
      call = call
    )
  }
  
  if (compose_coefficient) {
    output$coefficient <- .compose_coeff(
      paths = paths$coeff,
      coeff_extract = comp_extract$coefficient,
      sets = sets,
      time_steps = time_steps,
      call = call
    )
  }
  
  if (type == "all") {
    output <- rbind(output$variable, output$coefficient)
  } else {
    output <- output[[1]]
  }

  if (!is.null(name)) {
    if (length(name) > 1) {
      output <- output[output$name %in% name, ]
    } else {
      output <- purrr::pluck(output, "dat", name)
    }
  }
  
  return(output)
}
