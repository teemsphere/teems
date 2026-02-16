#' @importFrom purrr list_flatten
#' @keywords internal
#' @noRd
.load_aux <- function(aux,
                      data_type,
                      call) {
  check <- c("character", "data.frame")

  loaded <- list()
  nested <- FALSE

  if (!is.null(aux)) {
    for (idx in seq_along(aux)) {
      header <- names(aux)[idx]
      input <- aux[[idx]]

      if (!inherits(input, check)) {
        .cli_action(aux_err$invalid_input,
          action = "abort",
          call = call
        )
      }

      input <- .read_aux(
        input = input,
        header = header,
        data_type = data_type,
        call = call
      )
      loaded[[idx]] <- input

      if (inherits(input, "data.frame")) {
        names(loaded)[idx] <- header
      } else if (inherits(input, "list")) {
        nested <- TRUE
      }
    }
  } else {
    loaded <- NULL
  }

  if (nested) {
    loaded <- purrr::list_flatten(loaded)
  }

  return(loaded)
}