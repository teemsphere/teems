#' @keywords internal
#' @noRd
.read_aux <- function(input,
                      header,
                      data_type,
                      call) {
  UseMethod(".read_aux")
}

#' @keywords internal
#' @noRd
#' @method .read_aux data.frame
#' @export
.read_aux.data.frame <- function(input,
                                 header,
                                 data_type,
                                 call) {
  if (data_type %!=% "set") {
    col_nmes <- colnames(input)

    if (!"Value" %in% col_nmes) {
      .cli_action(aux_err$missing_col,
        action = "abort",
        call = call
      )
    }
    x_val_col <- col_nmes[!col_nmes %in% "Value"]
    dimnames <- purrr::map(x_val_col, function(c) {
      unique(input[[c]])
    })
    dim <- lengths(dimnames)
    names(dimnames) <- x_val_col

    input <- array(input$Value, dim = dim, dimnames = dimnames)
    class(input) <- c(header, data_type, class(input))
  } else {
    class(input) <- c(header, header, data_type, class(input))
  }
  return(input)
}

#' @importFrom tools file_path_sans_ext
#' @importFrom purrr  map
#' 
#' @keywords internal
#' @noRd
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