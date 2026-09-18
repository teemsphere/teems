#' The energy layers read the CDE parameters over TOPP under the names
#' SUBP/INCP; the database ships those under SUBE/INCE alongside the
#' COMM-dimensioned pair (which flexagg writes out as SUB1/INC1 and no
#' GTAP-E statement reads). The TOPP-dimensioned headers are bound to
#' the names the model reads and the incumbents dropped, rather than
#' relying on the two lengths differing.
#'
#' SUBPAR enters the CDE expenditure function as ALPHA = 1 - SUBPAR, so
#' it cannot exceed one. The eny row of the 11c releases is an
#' un-normalised sum and reaches five (GTAP-E) to fourteen
#' (GTAP-Power); nothing downstream checks a parameter's magnitude, so
#' it is refused here rather than solved as a silently wrong demand
#' system. INCPAR is an income parameter, legitimately above one, and
#' is not bounded.
#'
#' Subsetting drops the list's attributes; the caller restores them
#' ([.layer_finish()]).
#'
#' @keywords internal
#' @noRd
.layer_promote_cde <- function(i_data, call) {
  for (pair in list(c("SUBE", "SUBP"), c("INCE", "INCP"))) {
    from <- pair[[1]]
    to <- pair[[2]]
    nm <- toupper(names(i_data))
    i <- match(from, nm)
    a <- i_data[[i]]
    dn <- names(dimnames(a))
    if (is.null(dn) || !dn[[1]] %=% "TOPP") {
      e_header <- from
      e_dim <- if (is.null(dn)) {
        "unnamed"
      } else {
        dn[[1]]
      }
      .cli_action(data_err$e_topp_dim,
        action = "abort",
        call = call
      )
    }
    if (to %=% "SUBP") {
      e_max <- signif(max(a, na.rm = TRUE), 4)
      if (e_max > 1) {
        e_header <- from
        .cli_action(data_err$cde_range,
          action = c("abort", "inform"),
          call = call
        )
      }
    }
    class(a)[1] <- to
    i_data[[i]] <- a
    names(i_data)[i] <- to
    drop <- setdiff(which(toupper(names(i_data)) %in% to), i)
    if (length(drop) > 0L) {
      i_data <- i_data[-drop]
    }
  }
  return(i_data)
}
