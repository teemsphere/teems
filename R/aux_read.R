#' @keywords internal
#' @noRd
.read_aux <- function(input,
                      type,
                      header,
                      call) {
  UseMethod(".read_aux")
}

#' @keywords internal
#' @noRd
#' @method .read_aux data.frame
#' @export
.read_aux.data.frame <- function(input,
                                 type,
                                 header,
                                 call) {
  if (type %!=% "set") {
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
    class(input) <- c(header, type, class(input))
  } else {
    class(input) <- c(header, header, type, class(input))
  }
  return(input)
}

#' @keywords internal
#' @noRd
#' @method .read_aux character
#' @export
.read_aux.character <- function(input,
                                type,
                                header,
                                call) {

  if (length(input) %=% 1L) {
    input <- .read_input(
      input = input,
      data_type = type,
      call = call
    )

    if (inherits(input, "data.frame")) {
      input <- .read_aux(
        input = input,
        header = header,
        type = type,
        call = call
      )
    }
  }
  
  if (type %=% "set") {
    class(input) <- c(header, header, type, class(input))
  }
  
  if (!is.null(header)) {
    attr(input, "header") <- header
  }

  return(input)
}
