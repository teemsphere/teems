#' @keywords internal
#' @noRd
.merge_aux <- function(input,
                       input_name,
                       i_data,
                       aux_call,
                       data_call) {
  UseMethod(".merge_aux")
}

#' @importFrom purrr map_lgl map_chr
#' @method .merge_aux default
#' @keywords internal
#' @noRd
.merge_aux.default <- function(input,
                               input_name,
                               i_data,
                               aux_call,
                               data_call) {
  
  dim <- dim(input)
  dimnames <- dimnames(input)
  nme_dimnames <- names(dimnames)
  data_type <- class(input)[[2]]

  if (inherits(input, names(i_data))) {
    chk_dim <- dim(i_data[as.logical(inherits(input, names(i_data), TRUE))][[1]])
    chk_dimnames <- dimnames(i_data[as.logical(inherits(input, names(i_data), TRUE))][[1]])
    chk_nme_dimnames <- names(chk_dimnames)

    if (dim %!=% chk_dim) {
      .cli_action(aux_err$x_dim,
        action = c("abort", "inform"),
        call = data_call
      )
    }

    for (idx in seq_along(dimnames)) {
      i_d <- dimnames[[idx]]
      c_d <- chk_dimnames[[idx]]
      nme <- nme_dimnames[[idx]]
      if (!all(c_d %in% i_d)) {
        missing_nme <- setdiff(c_d, i_d)
        name <- nme
        .cli_action(aux_err$missing_dimname,
          action = c("abort", "inform"),
          call = data_call
        )
      }
    }

    if (nme_dimnames %!=% chk_nme_dimnames) {
      odd_name <- setdiff(nme_dimnames, chk_nme_dimnames)
      presumed_name <- chk_nme_dimnames[match(odd_name, nme_dimnames)]
      .cli_action(aux_wrn$nme_dimnames,
        action = c("warn", "inform"),
        call = data_call
      )
      names(dimnames(input)) <- chk_nme_dimnames
    }

    classes <- class(i_data[[input_name]])
    class(input) <- classes

    if (data_type %!=% classes[[2]]) {
      .cli_action(aux_wrn$diff_data_type,
        action = "warn",
        call = aux_call
      )

      class(input)[[2]] <- data_type
    }
  } else {
    rec_sets <- i_data[purrr::map_lgl(i_data, inherits, "set")]
    rec_set_names <- setdiff(purrr::map_chr(rec_sets, \(s) {
      class(s)[[2]]
    }), "set")
    for (idx in seq_along(nme_dimnames)) {
      s <- nme_dimnames[[idx]]
      s_input <- dimnames[[s]]
      if (s %in% rec_set_names) {
        s_chk <- rec_sets[purrr::map_lgl(rec_sets, inherits, s)][[1]]

        if (!all(s_chk %in% s_input)) {
          missing_nme <- setdiff(s_chk, s_input)
          .cli_action(aux_err$missing_dimname2,
            action = c("abort", "inform"),
            call = aux_call
          )
        }
      } else if (any(purrr::map_lgl(rec_sets, \(s) {
        all(s_input %in% s)
      }))) {
        pre_s <- s
        s <- class(rec_sets[purrr::map_lgl(rec_sets, \(s) {
          all(s_input %in% s)
        })][[1]])[[2]]
        .cli_action(aux_wrn$mod_dimname,
          action = "warn",
          call = data_call
        )
      } else {
        .cli_action(aux_err$invalid_dimname,
          action = "abort",
          call = aux_call
        )
      }
      nme_dimnames[[idx]] <- s
    }

    if (names(dimnames(input)) %!=% nme_dimnames && nme_dimnames %!=% character(0)) {
      names(dimnames(input)) <- nme_dimnames
    }

    exist_format <- unique(purrr::map_chr(i_data[purrr::map_lgl(i_data, inherits, "dat")], \(i) {
      class(i)[[3]]
    }))

    class(input) <- append(class(input), exist_format, 2)
  }
  i_data[[input_name]] <- input
  return(i_data)
}

#' @importFrom purrr map_lgl map_chr
#' @method .merge_aux set
#' @keywords internal
#' @noRd
.merge_aux.set <- function(input,
                           input_name,
                           i_data,
                           aux_call,
                           data_call) {

  exist_format <- unique(purrr::map_chr(i_data[purrr::map_lgl(i_data, inherits, "dat")], \(i) {
    class(i)[[3]]
  }))
  
  classes <- append(class(input), exist_format, 3)
  attributes(input) <- NULL
  input <- structure(input,
                     user_set = TRUE,
                     class = classes)
  
  i_data[[input_name]] <- input
  return(i_data)
}