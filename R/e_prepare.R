#' GTAP-E database preparation (GTAP_convert target "GTAP-E";
#' applied by ems_data when the layer is detected)
#'
#' The GTAP-E layer ships on a GTAPv7-format database (GTAPv12a,
#' v11c) with the energy sets COME, FUEL, CTAX, NCOA, the emissions
#' data CO2Q and MDF/MMF/MDP/MMP/MDG/MMG/MDI/MMI, and the CDE
#' parameters SUBE/INCE alongside the standard SUBP/INCP. The
#' GTAP-E model declares
#'   - the disaggregated commodity set DCOMM (DCOM), the mapping MCOM
#'     from DCOMM to COMM and the disaggregated electricity set DELY,
#'     which flexagg's aggdat_e.tab synthesizes and the database does
#'     not carry: DCOM is the commodity list at source resolution (the
#'     mapping composes onto the aggregated COMM at deploy,
#'     .finalize_map_data) and DELY = COME - FUEL;
#'   - the aggregated energy sets EGY and ENYP/ENYG/ENYI, all equal to
#'     COME: in GTAP-E the electricity nest node is the electricity
#'     commodity itself, so the energy subutility sets coincide with
#'     the energy commodities (GTAP-Power splits them, see the EP
#'     layer). These are COMM subsets and follow its aggregation;
#'   - INCPAR/SUBPAR over TOPP rather than COMM ("GTAP-E changed:
#'     Redefine from COMM to TOPP"). The database ships those under
#'     SUBE/INCE and the COMM-dimensioned pair under SUBP/INCP, which
#'     flexagg's aggpar_e renames to SUB1/INC1; SUBE/INCE are promoted
#'     into the names the model reads and the COMM-dimensioned
#'     incumbents dropped, since nothing in GTAP-E reads them.
#' TOPP itself is derived by the model (ENY + NENYP) and is not read
#' from file, but the SUBE/INCE dimension carries that name and
#' teems-R maps dimensions by name: without a TOPP entry the
#' dimension is silently passed through un-aggregated, so the set is
#' supplied here. Its elements are eny + (COMM - EGY), which
#' .infer_set_mappings resolves by routing the commodities through the
#' COMM mapping and giving eny an identity leg.
#' The disaggregated sets are marked user sets: they never aggregate.
#'
#' @keywords internal
#' @noRd
.is_e_input <- function(i_data) {
  nm <- toupper(names(i_data))
  all(c("COME", "SUBE", "INCE") %in% nm) &&
    # the GTAP-Power layer carries the same energy headers plus the
    # electricity technologies: that is the EP layer, not this one
    !any(c("ELEA", "ELEC", "POWR") %in% nm)
}

#' @keywords internal
#' @noRd
.prepare_e <- function(i_data, call) {
  metadata <- attr(i_data, "metadata")
  fmt <- metadata$data_format
  cls <- class(i_data)
  nm <- toupper(names(i_data))
  req <- c("COMM", "ACTS", "REG", "COME", "FUEL", "SUBE", "INCE")
  missing_e <- setdiff(req, nm)
  if (length(missing_e) > 0L) {
    .cli_action(data_err$e_incomplete,
      action = "abort",
      call = call
    )
  }
  set_of <- function(h) tolower(trimws(as.character(i_data[[match(h, nm)]])))
  comm <- set_of("COMM")
  come <- set_of("COME")
  fuel <- set_of("FUEL")

  mk_set <- function(header, ele, user_set = TRUE) {
    s <- ele
    class(s) <- c(header, header, "set", fmt, "character")
    if (user_set) {
      attr(s, "user_set") <- TRUE
    }
    s
  }
  new_sets <- list(
    DCOM = mk_set("DCOM", comm),
    MCOM = mk_set("MCOM", comm),
    DELY = mk_set("DELY", setdiff(come, fuel)),
    EGY = mk_set("EGY", come, user_set = FALSE),
    ENYP = mk_set("ENYP", come, user_set = FALSE),
    ENYG = mk_set("ENYG", come, user_set = FALSE),
    ENYI = mk_set("ENYI", come, user_set = FALSE),
    TOPP = mk_set("TOPP", c("eny", setdiff(comm, come)), user_set = FALSE)
  )
  new_sets <- new_sets[!names(new_sets) %in% nm]

  # taken before the reclass below, which would otherwise carry the
  # last set past the parameters and scatter the new sets among them
  at_set <- max(which(purrr::map_lgl(i_data, inherits, "set")), 0L)

  # BLOC and the REGTOBLOC mapping are the only character headers read
  # from GTAPPARM rather than GTAPSETS, so the loader classes them as
  # parameters and they would reach the numeric aggregation path. TRBL
  # carries the bloc elements, which the database ships one-to-one with
  # REG, so it follows that set's aggregation; MAPB is the mapping's
  # region list at source resolution, which .finalize_map_data composes
  # onto the aggregated BLOC at deploy, so it stays a user set.
  reclass_set <- function(header, user_set) {
    i <- match(header, nm)
    if (is.na(i)) {
      return(invisible(NULL))
    }
    s <- tolower(trimws(as.character(i_data[[i]])))
    class(s) <- c(header, header, "set", fmt, "character")
    if (user_set) {
      attr(s, "user_set") <- TRUE
    }
    i_data[[i]] <<- s
  }
  reclass_set("TRBL", user_set = FALSE)
  reclass_set("MAPB", user_set = TRUE)

  # the model reads the CDE parameters over TOPP under the names
  # SUBP/INCP; bind the TOPP-dimensioned headers to those names rather
  # than relying on the COMM-dimensioned incumbents' length differing
  promote <- function(from, to) {
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
    # SUBPAR enters the CDE expenditure function as ALPHA = 1 - SUBPAR,
    # so it cannot exceed one. The eny row of the 11c releases is an
    # un-normalised sum and reaches five (GTAP-E) to fourteen
    # (GTAP-Power); nothing downstream checks a parameter's magnitude,
    # so refuse it here rather than solve a silently wrong demand
    # system. INCPAR is an income parameter and is legitimately above
    # one, so it is not bounded here.
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
    i_data[[i]] <<- a
    names(i_data)[i] <<- to
    nm[i] <<- to
    # the COMM-dimensioned pair (flexagg writes it out as SUB1/INC1) is
    # read by no GTAP-E statement
    j <- which(nm %in% to)
    if (length(j) > 1L) {
      drop <- setdiff(j, i)
      i_data <<- i_data[-drop]
      nm <<- nm[-drop]
    }
  }
  promote("SUBE", "SUBP")
  promote("INCE", "INCP")

  # keep the set / par / dat grouping: new sets after the last set
  attrs <- attributes(i_data)
  out <- unclass(i_data)
  out <- append(out, new_sets, after = at_set)
  for (a in setdiff(names(attrs), c("names", "class", "metadata"))) {
    attr(out, a) <- attrs[[a]]
  }
  metadata$e <- TRUE
  attr(out, "metadata") <- metadata
  class(out) <- cls
  out
}
