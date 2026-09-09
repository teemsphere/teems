#' @importFrom purrr map_lgl
#'
#' @keywords internal
#' @noRd
.implement_convert <- function(args_list,
                               call) {
  v <- .validate_convert_args(
    a = args_list,
    call = call
  )

  i_data <- .load_input_data(
    dat_input = v$dat_input,
    par_input = v$par_input,
    set_input = v$set_input,
    data_call = call,
    convert = TRUE
  )

  if (!is.null(v$target) && v$target %=% "GTAP-E") {
    # the GTAP-E layer on the GTAPv7 format: a v6-format database
    # (GTAP 10a E) carries different headers and is not prepared
    if (isTRUE(attr(i_data, "metadata")$e)) {
      target <- v$target
      .cli_action(convert_wrn$format,
        action = "warn",
        call = call
      )
    } else {
      if (attr(i_data, "metadata")$data_format %=% "GTAPv6") {
        .cli_action(data_err$e_v6_format,
          action = "abort",
          call = call
        )
      }
      i_data <- .prepare_e(i_data = i_data, call = call)
    }
    v$target <- NULL
  } else if (!is.null(v$target) && v$target %=% "GTAP-AEZ") {
    # the GTAP-AEZ layer on the GTAPv7 format: a v6-format database
    # (GTAPv10a AEZ) converts to v7 first
    if (isTRUE(attr(i_data, "metadata")$aez)) {
      target <- v$target
      .cli_action(convert_wrn$format,
        action = "warn",
        call = call
      )
    } else {
      if (attr(i_data, "metadata")$data_format %=% "GTAPv6") {
        # the v6-era AEZ layer (GTAP 10a: ESBL/ETL*/ETA/YD01/YDEL over
        # LAND_COMM/ENDWL_COMM/PROD_COMM, no LUSA) carries different
        # headers from the v7-format layer this target prepares
        .cli_action(data_err$aez_v6_format,
          action = "abort",
          call = call
        )
      }
      i_data <- .prepare_aez(i_data = i_data, call = call)
    }
    v$target <- NULL
  } else if (attr(i_data, "metadata")$data_format %=% v$target) {
    target <- v$target
    .cli_action(convert_wrn$format,
      action = "warn",
      call = call
    )

    v$target <- NULL
  }

  if (!is.null(v$target)) {
    i_data <- .convert_data(i_data = i_data)
  }

  dat <- i_data[purrr::map_lgl(i_data, \(i) {
    inherits(i, "dat")
  })]
  attr(dat, "metadata") <- attr(i_data, "metadata")

  par <- i_data[purrr::map_lgl(i_data, \(i) {
    inherits(i, "par")
  })]

  set <- i_data[purrr::map_lgl(i_data, \(i) {
    inherits(i, "set")
  })]

  i_data <- list(
    dat = dat,
    par = par,
    set = set
  )
  return(i_data)
}