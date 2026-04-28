#' @keywords internal
#' @noRd
.reduce2sets <- function(preswap,
                         swap,
                         ...) {
  UseMethod(".reduce2sets")
}

#' @importFrom data.table fsetequal
#' 
#' @noRd
#' @keywords internal
#' @method .reduce2sets ele
#' @export
.reduce2sets.ele <- function(preswap,
                             swap,
                             ...) {
  if (!data.table::fsetequal(attr(preswap, "ele"), attr(swap, "ele"))) {
    .cli_action("Internal error on an ele to ele swap-out.",
      action = "abort",
      .internal = TRUE
    )
  }
  return(NULL)
}