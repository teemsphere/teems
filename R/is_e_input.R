#' Does this database carry the GTAP-E layer? Detection is by the
#' headers the layer adds, not by the file name.
#' @keywords internal
#' @noRd
.is_e_input <- function(i_data) {
  nm <- toupper(names(i_data))
  return(all(c("COME", "SUBE", "INCE") %in% nm) &&
    # the GTAP-Power layer carries the same energy headers plus the
    # electricity technologies: that is the EP layer, not this one
    !any(c("ELEA", "ELEC", "POWR") %in% nm))
}
