#' @importFrom purrr pwalk map_lgl map_chr
#' @keywords internal
#' @noRd
.merge_aux <- function(aux_input,
                       i_data,
                       aux_call,
                       data_call) {
  for (aux in seq_along(aux_input)) {
    input <- aux_input[[aux]]
    dim <- dim(input)
    dimnames <- dimnames(input)
    nme_dimnames <- names(dimnames)
    input_name <- class(input)[[1]]

    if (inherits(input, names(i_data))) {
      chk_dim <- dim(i_data[as.logical(inherits(input, names(i_data), TRUE))][[1]])
      chk_dimnames <- dimnames(i_data[as.logical(inherits(input, names(i_data), TRUE))][[1]])
      chk_nme_dimnames <- names(chk_dimnames)

      if (!dim %=% chk_dim) {
        .cli_action(aux_err$x_dim,
          action = c("abort", "inform"),
          call = aux_call
        )
      }

      purrr::pwalk(
        list(
          dimnames,
          chk_dimnames,
          nme_dimnames
        ),
        function(i_d, c_d, nme) {
          if (!all(c_d %in% i_d)) {
            missing_nme <- setdiff(c_d, i_d)
            name <- nme
            .cli_action(aux_err$missing_dimname,
              action = c("abort", "inform"),
              call = aux_call
            )
          }
        }
      )

      if (!nme_dimnames %=% chk_nme_dimnames) {
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
    } else {
      rec_sets <- i_data[purrr::map_lgl(i_data, inherits, "set")]
      rec_set_names <- setdiff(purrr::map_chr(rec_sets, \(s) {
        class(s)[[2]]
      }), "set")
      nme_dimnames <- purrr::map_chr(nme_dimnames, \(s) {
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
        } else {
          if (any(purrr::map_lgl(rec_sets, \(s) {
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
            # so here we need to be able to also load in a novel unaggregated set list as an option
            # rather obscure use-case but noted
            .cli_action(aux_err$invalid_dimname,
              action = "abort",
              call = aux_call
            )
          }
        }
        return(s)
      })

      if (!names(dimnames(input)) %=% nme_dimnames && !nme_dimnames %=% character(0)) {
        names(dimnames(input)) <- nme_dimnames
      }
      # get datatype from dat
      inferred_data_type <- unique(purrr::map_chr(i_data[purrr::map_lgl(i_data, inherits, "dat")], \(i) {
        class(i)[[3]]
      }))

      class(input) <- append(class(input), inferred_data_type, 2)
    }
    i_data[[input_name]] <- input
  }
  return(i_data)
}