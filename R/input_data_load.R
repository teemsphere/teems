#' @importFrom purrr map
#' 
#' @noRd
#' @keywords internal
.load_input_data <- function(dat_input,
                             par_input,
                             set_input,
                             aux_input,
                             target_format,
                             call) {

  dat <- .read_input(
    input = dat_input,
    attach_metadata = TRUE
  )
  metadata <- attr(dat, "metadata")

  if (metadata$data_format %=% target_format) {
    .cli_action(data_wrn$unnecessary_cvrt,
      action = "warn",
      call = call
    )
  }
  par <- .read_input(
    input = par_input,
    data_format = metadata$data_format
  )
  set <- .read_input(
    input = set_input,
    data_format = metadata$data_format
  )

  if (!inherits(dat, "dat")) {
    inferred_type <- attr(dat, "data_type")
    .cli_action(data_err$invalid_dat_har,
                action = "abort",
                call = call
    )
  }
  
  if (!inherits(par, "par")) {
    inferred_type <- attr(par, "data_type")
    .cli_action(data_err$invalid_par_har,
                action = "abort",
                call = call
    )
  }
  
  if (!inherits(set, "set")) {
    inferred_type <- attr(set, "data_type")
    .cli_action(data_err$invalid_set_har,
                action = "abort",
                call = call
    )
  }
  
  i_data <- c(set, par, dat)
  
  if (!is.null(aux_input)) {
    aux <- .read_input(
      input = aux_input,
      data_format = metadata$data_format
    )

    i_data <- c(i_data, aux)
  }
  
  i_data <- i_data[!names(i_data) %in% .o_full_exclude()]

  # remove duplicates precedence aux, dat, par, set
  if (any(duplicated(names(i_data)))) {
    i_data <- i_data[!duplicated(names(i_data), fromLast = TRUE)]
  }
  
  i_data <- lapply(
    i_data,
    function(h) {
      if (inherits(h, c("dat", "par")) && !is.null(dimnames(h))) {
        dimnames(h) <- purrr::map(dimnames(h), function(e) {
          tolower(gsub("CGDS", "zcgds", e, ignore.case = TRUE))
        })
      } else if (inherits(h, "set")) {
        h <- tolower(gsub(
          "CGDS",
          "zcgds",
          h,
          ignore.case = TRUE
        ))
      }
      return(h)
    }
  )

  .check_database_version(
    vetted = c("GTAPv9A", "GTAPv10A", "GTAPv11c"),
    provided = metadata$full_database_version,
    call = call
  )
  
  if (.o_verbose()) {
  .inform_metadata(metadata = metadata)
  }
  
  attr(i_data, "metadata") <- metadata
  class(i_data) <- c(metadata$data_format, class(i_data))
  return(i_data)
}