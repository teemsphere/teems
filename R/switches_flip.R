#' @keywords internal
#' @noRd
.flip_switches <- function(model,
                           switches,
                           switch_names) {

  r_idx <- match(switch_names, model$comp1)
  model$comp2[r_idx] <- switches
  model$tab[r_idx] <- paste(
    model$type[r_idx],
    model$qualifier_list[r_idx],
    model$comp1[r_idx],
    paste0(switches, ";")
  )
  return(model)
}