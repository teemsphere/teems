.read_aux <- function(input,
                      header,
                      data_type,
                      call) {
  UseMethod(".read_aux")
}

#' @method .read_aux data.frame
#' @export
.read_aux.data.frame <- function(input,
                                 header,
                                 data_type,
                                 call) {
  col_nmes <- colnames(input)

  if (!"Value" %in% col_nmes) {
    .cli_action(aux_err$missing_col,
      action = "abort",
      call = call
    )
  }
  x_val_col <- col_nmes[!col_nmes %in% "Value"]

  if (!inherits(input, "data.table")) {
    data.table::setDT(input, key = x_val_col)
  } else {
    data.table::setkeyv(input, x_val_col)
  }

  if (!data_type %=% "set") {
    class(input) <- c(header, data_type, class(input))
  } else {
    class(input) <- c(header, header, data_type, class(input))
  }
  return(input)
}

#' @method .read_aux character
#' @export
.read_aux.character <- function(input,
                                header,
                                data_type,
                                call) {
  input <- .check_input(
    file = input,
    valid_ext = c("csv", "har"),
    call = call
  )

  if (inherits(input, "har")) {
    basename <- tools::file_path_sans_ext(basename(input))
  }

  if (!is.null(header)) {
    attr(input, "header") <- header
  }

  input <- .read_input(
    input = input,
    data_type = data_type,
    call = call
  )

  if (inherits(input, "data.frame")) {
    input <- .read_aux(
      input = input,
      header = header,
      data_type = data_type,
      call = call
    )
  } else if (inherits(input, "list")) {
    # if har origin
    input <- purrr::map(input, function(i) {
      attr(i, "name") <- basename
      return(i)
    })
  }

  return(input)
}