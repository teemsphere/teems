#' @importFrom purrr map_lgl
#' @keywords internal
#' @noRd
.split_generic_headers <- function(dat_input) {
  metadata <- attr(dat_input, "metadata")
  is_char <- purrr::map_lgl(dat_input, is.character)
  is_history <- startsWith(toupper(names(dat_input)), "XX")
  is_set <- is_char & !is_history & purrr::map_lgl(dat_input, \(h) {
    e <- trimws(as.vector(h))
    length(e) > 0L && !anyDuplicated(e)
  })
  keep <- !is_char | is_set
  i_data <- dat_input[keep]
  for (nme in names(i_data)[is_set[keep]]) {
    e <- trimws(as.vector(i_data[[nme]]))
    class(e) <- c(nme, nme, "set", metadata$data_format, "character")
    i_data[[nme]] <- e
  }
  metadata$generic <- TRUE
  metadata$set_names <- names(i_data)[is_set[keep]]
  metadata$n_sets <- length(metadata$set_names)
  metadata$n_headers <- sum(!is_set[keep])
  metadata$full_database_version <- "generic"
  attr(i_data, "metadata") <- metadata
  return(i_data)
}

#' @importFrom rlang current_env
#' @keywords internal
#' @noRd
.inform_generic <- function(metadata) {
  n_sets <- metadata$n_sets
  set_names <- metadata$set_names
  n_headers <- metadata$n_headers
  .cli_action(data_info$generic,
    action = c("inform", "inform")
  )
  return(invisible(NULL))
}
