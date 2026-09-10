#' GTAP-Power database preparation (GTAP_convert target "GTAP-EP";
#' applied by ems_data when the layer is detected)
#'
#' The GTAP-Power layer ships on a GTAPv7-format database (GTAPv12a,
#' v11c) that replaces the single electricity activity with transmission
#' and distribution plus eleven generation technologies, so COMM and
#' ACTS carry 76 elements rather than 65. It is GTAP-E plus the
#' electricity nests: the energy sets, carbon accounting, trading blocs
#' and the CDE parameters over TOPP are all as in [.prepare_e()], and
#' the twenty extra sets below describe the electricity subutilities.
#'
#' The model reads 27 set headers the database does not carry, which
#' flexagg's aggdat_e.tab synthesizes at aggregation. Their construction
#' is pinned against the aggregated reference the distributed test data
#' ships (teems_tabs/official/GTAPv7-EP/sets.har):
#'   - DCOM, MCOM, DELY as in GTAP-E, but DELY is the whole
#'     source-resolution electricity list (T&D and the technologies),
#'     not a single commodity;
#'   - EGY = FUEL + ELEC. NOT COME, which GTAP-E can use: flexagg
#'     redefines COME across the aggregation step, so that at source it
#'     excludes T&D while on the aggregated file it includes it. Taking
#'     COME here would drop T&D into NENYP and hence into TOPP,
#'     silently;
#'   - ENYP/ENYG/ENYI = FUEL + the synthetic nest node `ely`, so unlike
#'     GTAP-E they are NOT equal to EGY: in GTAP-E the electricity nest
#'     node is the electricity commodity itself, in GTAP-Power it is a
#'     node above the three aggregated electricity commodities;
#'   - the nest node lists ELE* (ely, egen, ebl, epl), ELY* (egen, TnD)
#'     and EGN* (ebl, epl), which are the same at every aggregation;
#'   - EBL*/EPL*, the base-load and peak-load technologies. The split is
#'     read from the technology names, which carry a consistent BL/P
#'     suffix; the long labels are inconsistent ("Coal power baseload"
#'     against "Hydro power base load", with nuclear, wind and solar
#'     unmarked) and cannot serve as a fallback, so a list that does not
#'     partition cleanly is a named abort rather than a guess.
#' Each of the four nest families has one copy per demand agent (the
#' F/G/I/P suffix: firm, government, investment, private) with identical
#' contents.
#'
#' An aggregation that merges a base-load technology with a peak-load
#' one makes EBL* and EPL* overlap and the generation nest ill-defined,
#' which is why the shipped GTAP-EP mappings are purpose-built rather
#' than lifted from the standard ones: every standard mapping sends the
#' electricity activities to a single bucket.
#'
#' @keywords internal
#' @noRd
.is_ep_input <- function(i_data) {
  nm <- toupper(names(i_data))
  all(c("COME", "SUBE", "INCE") %in% nm) &&
    # the electricity technologies are what separates the Power layer
    # from the energy-only one
    all(c("ELEA", "ELEC", "POWR") %in% nm)
}

#' The base-load and peak-load technologies of an electricity activity
#' list, split on the name suffix. Returns NULL when the suffix does not
#' partition the list, the caller aborting by name.
#'
#' @keywords internal
#' @noRd
.ep_load_split <- function(techs) {
  bl <- techs[grepl("bl$", techs, ignore.case = TRUE)]
  pl <- techs[grepl("[^b]p$", techs, ignore.case = TRUE)]
  if (length(bl) == 0L || length(pl) == 0L ||
    length(intersect(bl, pl)) > 0L ||
    !setequal(c(bl, pl), techs)) {
    return(NULL)
  }
  list(base = bl, peak = pl)
}

#' @keywords internal
#' @noRd
.prepare_ep <- function(i_data, call) {
  metadata <- attr(i_data, "metadata")
  fmt <- metadata$data_format
  cls <- class(i_data)
  nm <- toupper(names(i_data))
  req <- c("COMM", "ACTS", "REG", "COME", "FUEL", "ELEC", "ELEA", "SUBE", "INCE")
  missing_ep <- setdiff(req, nm)
  if (length(missing_ep) > 0L) {
    .cli_action(data_err$ep_incomplete,
      action = "abort",
      call = call
    )
  }
  set_of <- function(h) tolower(trimws(as.character(i_data[[match(h, nm)]])))
  comm <- set_of("COMM")
  fuel <- set_of("FUEL")
  elec <- set_of("ELEC")
  techs <- set_of("ELEA")

  split <- .ep_load_split(techs)
  if (is.null(split)) {
    ep_techs <- techs
    .cli_action(data_err$ep_load_split,
      action = "abort",
      call = call
    )
  }

  # EGY is the fuels together with the whole electricity list; the nest
  # sets read the synthetic node names the model builds above them
  egy <- union(fuel, elec)
  eny <- c(fuel, "ely")
  ele_nodes <- c("ely", "egen", "ebl", "epl")
  ely_nodes <- c("egen", setdiff(elec, techs))
  egn_nodes <- c("ebl", "epl")

  mk_set <- function(header, ele, user_set = TRUE) {
    s <- ele
    class(s) <- c(header, header, "set", fmt, "character")
    if (user_set) {
      attr(s, "user_set") <- TRUE
    }
    s
  }
  # the disaggregated commodity list and its mapping stay at source; the
  # energy and nest sets follow COMM, the node lists being aggregation
  # invariant either way
  new_sets <- list(
    DCOM = mk_set("DCOM", comm),
    MCOM = mk_set("MCOM", comm),
    DELY = mk_set("DELY", elec),
    EGY = mk_set("EGY", egy, user_set = FALSE),
    TOPP = mk_set("TOPP", c("eny", setdiff(comm, egy)), user_set = FALSE)
  )
  for (agent in c("P", "G", "I")) {
    new_sets[[paste0("ENY", agent)]] <-
      mk_set(paste0("ENY", agent), eny, user_set = FALSE)
  }
  for (agent in c("F", "G", "I", "P")) {
    new_sets[[paste0("ELE", agent)]] <-
      mk_set(paste0("ELE", agent), ele_nodes)
    new_sets[[paste0("ELY", agent)]] <-
      mk_set(paste0("ELY", agent), ely_nodes)
    new_sets[[paste0("EGN", agent)]] <-
      mk_set(paste0("EGN", agent), egn_nodes)
    new_sets[[paste0("EBL", agent)]] <-
      mk_set(paste0("EBL", agent), split$base, user_set = FALSE)
    new_sets[[paste0("EPL", agent)]] <-
      mk_set(paste0("EPL", agent), split$peak, user_set = FALSE)
  }
  new_sets <- new_sets[!names(new_sets) %in% nm]

  # taken before the reclass below, which would otherwise carry the last
  # set past the parameters and scatter the new sets among them
  at_set <- max(which(purrr::map_lgl(i_data, inherits, "set")), 0L)

  # BLOC and the REGTOBLOC mapping are read from GTAPPARM rather than
  # GTAPSETS, so the loader classes them as parameters and they would
  # reach the numeric aggregation path; as in the GTAP-E layer
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
  # SUBP/INCP, the database shipping them as SUBE/INCE
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
    j <- which(nm %in% to)
    if (length(j) > 1L) {
      drop <- setdiff(j, i)
      i_data <<- i_data[-drop]
      nm <<- nm[-drop]
    }
  }
  promote("SUBE", "SUBP")
  promote("INCE", "INCP")

  attrs <- attributes(i_data)
  out <- unclass(i_data)
  out <- append(out, new_sets, after = at_set)
  for (a in setdiff(names(attrs), c("names", "class", "metadata"))) {
    attr(out, a) <- attrs[[a]]
  }
  metadata$ep <- TRUE
  attr(out, "metadata") <- metadata
  class(out) <- cls
  out
}
