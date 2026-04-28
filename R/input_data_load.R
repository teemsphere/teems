#' @importFrom purrr map
#'
#' @noRd
#' @keywords internal
.load_input_data <- function(dat_input,
                             par_input,
                             set_input,
                             target_format,
                             data_call) {
  dat <- .read_input(
    input = dat_input,
    data_type = "dat",
    attach_metadata = TRUE
  )

  metadata <- attr(dat, "metadata")

  if (metadata$data_format %=% target_format) {
    .cli_action(data_wrn$unnecessary_cvrt,
      action = "warn",
      call = data_call
    )
  }
  par <- .read_input(
    input = par_input,
    data_type = "par",
    metadata = metadata
  )
  set <- .read_input(
    input = set_input,
    data_type = "set",
    metadata = metadata
  )

  i_data <- c(set, par, dat)
  i_data <- i_data[!names(i_data) %in% .o_full_exclude()]

  if (any(duplicated(names(i_data)))) {
    i_data <- i_data[!duplicated(names(i_data), fromLast = TRUE)]
  }

  .check_database_version(
    vetted = vetted_db_versions,
    provided = metadata$full_database_version,
    call = data_call
  )

  if (.o_verbose()) {
    .inform_metadata(metadata = metadata)
  }

  attr(i_data, "metadata") <- metadata
  class(i_data) <- c(metadata$data_format, class(i_data))
  return(i_data)
}