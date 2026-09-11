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
  attrs <- attributes(i_data)
  fmt <- attrs[["metadata"]][["data_format"]]
  nm <- toupper(names(i_data))
  req <- c("COMM", "ACTS", "REG", "COME", "FUEL", "ELEC", "ELEA", "SUBE", "INCE")
  missing_ep <- setdiff(req, nm)
  if (length(missing_ep) > 0L) {
    .cli_action(data_err$ep_incomplete,
      action = "abort",
      call = call
    )
  }
  comm <- .layer_elements(i_data, "COMM")
  fuel <- .layer_elements(i_data, "FUEL")
  elec <- .layer_elements(i_data, "ELEC")
  techs <- .layer_elements(i_data, "ELEA")

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

  # the disaggregated commodity list and its mapping stay at source; the
  # energy and nest sets follow COMM, the node lists being aggregation
  # invariant either way
  new_sets <- list(
    DCOM = .layer_set("DCOM", comm, fmt),
    MCOM = .layer_set("MCOM", comm, fmt),
    DELY = .layer_set("DELY", elec, fmt),
    EGY = .layer_set("EGY", egy, fmt, user_set = FALSE),
    TOPP = .layer_set("TOPP", c("eny", setdiff(comm, egy)), fmt, user_set = FALSE)
  )
  for (agent in c("P", "G", "I")) {
    h <- paste0("ENY", agent)
    new_sets[[h]] <- .layer_set(h, eny, fmt, user_set = FALSE)
  }
  for (agent in c("F", "G", "I", "P")) {
    nest <- list(
      ELE = list(ele_nodes, TRUE),
      ELY = list(ely_nodes, TRUE),
      EGN = list(egn_nodes, TRUE),
      EBL = list(split$base, FALSE),
      EPL = list(split$peak, FALSE)
    )
    for (family in names(nest)) {
      h <- paste0(family, agent)
      new_sets[[h]] <- .layer_set(h, nest[[family]][[1]], fmt,
        user_set = nest[[family]][[2]]
      )
    }
  }
  new_sets <- new_sets[!names(new_sets) %in% nm]

  i_data <- .layer_reclass_blocs(i_data, fmt)
  i_data <- .layer_promote_cde(i_data, call = call)
  .layer_finish(i_data, new_sets, attrs = attrs, flag = "ep")
}
