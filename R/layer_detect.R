#' @keywords internal
#' @noRd
.layer_detect <- function(i_data, flag) {
  d <- layer_spec[[flag]]$detect
  nm <- toupper(names(i_data))
  detected <- all(d$all %in% nm) &&
    (length(d$any) == 0L || any(d$any %in% nm)) &&
    (length(d$none) == 0L || !any(d$none %in% nm))
  return(detected)
}
