#' Shared mechanics of the model-layer database preparations
#'
#' The GTAP-AEZ, GTAP-E and GTAP-EP layers ([.prepare_aez()],
#' [.prepare_e()], [.prepare_ep()]) each synthesize the set headers
#' flexagg would build at aggregation and bind the parameter headers
#' the model reads under other names. The set construction, the bloc
#' header reclass, the CDE parameter promotion and the final append
#' are the same in every layer and live here; the layer files hold
#' only the element lists.
#'
#' @keywords internal
#' @noRd
NULL

#' Elements of the set header `h` (case-insensitive), lowercased
#'
#' @keywords internal
#' @noRd
.layer_elements <- function(i_data, h) {
  i <- match(h, toupper(names(i_data)))
  tolower(trimws(as.character(i_data[[i]])))
}

#' A set header carrying `ele`. A user set never aggregates (the
#' disaggregated lists and mappings the model reads at source
#' resolution); the rest follow the mapping of the set they subset.
#'
#' @keywords internal
#' @noRd
.layer_set <- function(header, ele, fmt, user_set = TRUE) {
  s <- ele
  class(s) <- c(header, header, "set", fmt, "character")
  if (user_set) {
    attr(s, "user_set") <- TRUE
  }
  s
}

#' BLOC and the REGTOBLOC mapping are the only character headers read
#' from GTAPPARM rather than GTAPSETS, so the loader classes them as
#' parameters and they would reach the numeric aggregation path. TRBL
#' carries the bloc elements, which the database ships one-to-one with
#' REG, so it follows that set's aggregation; MAPB is the mapping's
#' region list at source resolution, which .finalize_map_data composes
#' onto the aggregated BLOC at deploy, so it stays a user set.
#'
#' @keywords internal
#' @noRd
.layer_reclass_blocs <- function(i_data, fmt) {
  nm <- toupper(names(i_data))
  for (header in c("TRBL", "MAPB")) {
    i <- match(header, nm)
    if (is.na(i)) {
      next
    }
    i_data[[i]] <- .layer_set(
      header, tolower(trimws(as.character(i_data[[i]]))), fmt,
      user_set = header %=% "MAPB"
    )
  }
  i_data
}

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
      e_dim <- if (is.null(dn)) "unnamed" else dn[[1]]
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
  i_data
}

#' Append the synthesized headers and mark the layer prepared. The
#' set / par / dat grouping is kept: new sets go after the leading
#' block of sets (the reclassed bloc headers sit among the parameters
#' and are not that block's end), new parameters after the last
#' parameter. `attrs` are the input list's attributes, taken before
#' any subsetting dropped them; the metadata flag `flag` marks the
#' layer prepared so the hooks do not run twice.
#'
#' @keywords internal
#' @noRd
.layer_finish <- function(i_data, new_sets, new_par = list(), attrs, flag) {
  out <- unclass(i_data)
  is_set <- purrr::map_lgl(out, inherits, "set")
  at_set <- if (isTRUE(is_set[[1]])) rle(is_set)$lengths[[1]] else 0L
  out <- append(out, new_sets, after = at_set)
  if (length(new_par) > 0L) {
    at_par <- max(which(purrr::map_lgl(out, inherits, "par")), 0L)
    out <- append(out, new_par, after = at_par)
  }
  for (a in setdiff(names(attrs), c("names", "class", "metadata"))) {
    attr(out, a) <- attrs[[a]]
  }
  metadata <- attrs[["metadata"]]
  metadata[[flag]] <- TRUE
  attr(out, "metadata") <- metadata
  class(out) <- attrs[["class"]]
  out
}
