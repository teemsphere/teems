# GTAP-E database preparation (GTAP_convert target "GTAP-E";
# applied by ems_data when the layer is detected)
#
# The GTAP-E layer ships on a GTAPv7-format database (GTAPv12a,
# v11c) with the energy sets COME, FUEL, CTAX, NCOA, the emissions
# data CO2Q and MDF/MMF/MDP/MMP/MDG/MMG/MDI/MMI, and the CDE
# parameters SUBE/INCE alongside the standard SUBP/INCP. The
# GTAP-E model declares
#   - the disaggregated commodity set DCOMM (DCOM), the mapping MCOM
#     from DCOMM to COMM and the disaggregated electricity set DELY,
#     which flexagg's aggdat_e.tab synthesizes and the database does
#     not carry: DCOM is the commodity list at source resolution (the
#     mapping composes onto the aggregated COMM at deploy,
#     .finalize_map_data) and DELY = COME - FUEL;
#   - the aggregated energy sets EGY and ENYP/ENYG/ENYI, all equal to
#     COME: in GTAP-E the electricity nest node is the electricity
#     commodity itself, so the energy subutility sets coincide with
#     the energy commodities (GTAP-Power splits them, see the EP
#     layer). These are COMM subsets and follow its aggregation;
#   - INCPAR/SUBPAR over TOPP rather than COMM ("GTAP-E changed:
#     Redefine from COMM to TOPP"), bound from SUBE/INCE by
#     [.layer_promote_cde()].
# TOPP itself is derived by the model (ENY + NENYP) and is not read
# from file, but the SUBE/INCE dimension carries that name and
# teems-R maps dimensions by name: without a TOPP entry the
# dimension is silently passed through un-aggregated, so the set is
# supplied here. Its elements are eny + (COMM - EGY), which
# .infer_set_mappings resolves by routing the commodities through the
# COMM mapping and giving eny an identity leg.
# The disaggregated sets are marked user sets: they never aggregate.
# The 11c releases carry an un-normalised SUBE row and are refused by
# name; the 12a releases prepare.

#' @keywords internal
#' @noRd
.prepare_e <- function(i_data, call) {
  attrs <- attributes(i_data)
  fmt <- attrs[["metadata"]][["data_format"]]
  nm <- toupper(names(i_data))
  req <- c("COMM", "ACTS", "REG", "COME", "FUEL", "SUBE", "INCE")
  missing_e <- setdiff(req, nm)
  if (length(missing_e) > 0L) {
    .cli_action(data_err$e_incomplete,
      action = "abort",
      call = call
    )
  }
  comm <- .layer_elements(i_data, "COMM")
  come <- .layer_elements(i_data, "COME")
  fuel <- .layer_elements(i_data, "FUEL")

  new_sets <- list(
    DCOM = .layer_set("DCOM", comm, fmt),
    MCOM = .layer_set("MCOM", comm, fmt),
    DELY = .layer_set("DELY", setdiff(come, fuel), fmt),
    EGY = .layer_set("EGY", come, fmt, user_set = FALSE),
    ENYP = .layer_set("ENYP", come, fmt, user_set = FALSE),
    ENYG = .layer_set("ENYG", come, fmt, user_set = FALSE),
    ENYI = .layer_set("ENYI", come, fmt, user_set = FALSE),
    TOPP = .layer_set("TOPP", c("eny", setdiff(comm, come)), fmt, user_set = FALSE)
  )
  new_sets <- new_sets[!names(new_sets) %in% nm]

  i_data <- .layer_reclass_blocs(i_data, fmt)
  i_data <- .layer_promote_cde(i_data, call = call)
  prepared <- .layer_finish(i_data, new_sets, attrs = attrs, flag = "e")
  return(prepared)
}
