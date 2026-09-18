#' Does this database carry the GTAP-Power layer? Detection is by
#' the headers the layer adds, not by the file name.
#' @keywords internal
#' @noRd
.is_ep_input <- function(i_data) {
  nm <- toupper(names(i_data))
  return(all(c("COME", "SUBE", "INCE") %in% nm) &&
    # the electricity technologies are what separates the Power layer
    # from the energy-only one
    all(c("ELEA", "ELEC", "POWR") %in% nm))
}
