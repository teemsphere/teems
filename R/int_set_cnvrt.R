#' @importFrom data.table data.table 
#'
#' @keywords internal
#' @noRd
.convert_int_sets <- function(expr,
                              n_timestep,
                              n_timestep_coeff) {

  terms <- strsplit(expr, "-|\\s*-\\s*")[[1]]
  if (grepl("P\\[.*\\]\\s*-\\s*P\\[", expr)) {
    terms <- strsplit(expr, "\\s*-\\s*(?=P\\[)",
      perl = TRUE
    )[[1]]
    start <- .convert_p_term(
      term = terms[1],
      n_timestep = n_timestep,
      n_timestep_coeff = n_timestep_coeff
    )
    end <- .convert_p_term(
      term = terms[2],
      n_timestep = n_timestep,
      n_timestep_coeff = n_timestep_coeff
    )
    num_vec <- c(start:end)
  } else {
    num_vec <- .convert_p_term(
      term = expr,
      n_timestep = n_timestep,
      n_timestep_coeff = n_timestep_coeff
    )
  }

  mapping <- data.table::data.table(
    origin = num_vec,
    mapping = num_vec,
    key = c("origin", "mapping")
  )
  return(mapping)
}

.convert_p_term <- function(term,
                            n_timestep,
                            n_timestep_coeff) {

  content <- gsub(
    "P\\[|\\]",
    "",
    term
  )
  content <- gsub(
    n_timestep_coeff,
    as.character(n_timestep),
    content
  )
  eval(parse(text = content))
}
