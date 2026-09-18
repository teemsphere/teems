# PostSim sections (GEMPACK ch.12): tag the region, validate its
# contents, drop the markers. Declarations stay in place (the solver
# separates PostSim by execution order, not by namespace);
# .finalize_tab() re-wraps the executables in a trailing section.
#' @keywords internal
#' @noRd
.flag_tab_postsim <- function(tab,
                              ps_decl_names,
                              call) {
  is_marker <- tolower(tab$type) %in% "postsim"
  if (any(is_marker)) {
    ps_begin <- is_marker & grepl("begin", tab$tab, ignore.case = TRUE)
    ps_end <- is_marker & grepl("end", tab$tab, ignore.case = TRUE)
    tab$postsim <- (cumsum(ps_begin) - cumsum(ps_end)) > 0 & !ps_begin
    ps_allowed <- c(
      "set", "subset", "coefficient", "file", "mapping",
      "read", "formula", "assertion", "zerodivide"
    )
    ps_bad <- tab$postsim & !is_marker &
      !tolower(tab$type) %in% ps_allowed
    if (any(ps_bad)) {
      ps_bad_types <- unique(tab$type[ps_bad])
      .cli_action(model_err$postsim_invalid,
        action = c("abort", "inform"),
        call = call
      )
    }
    tab <- tab[!is_marker, ]
  } else {
    tab$postsim <- FALSE
  }
  if (length(ps_decl_names) > 0) {
    ps_decl <- tolower(tab$type) %in% c("coefficient", "set", "subset", "file") &
      toupper(tab$name) %in% ps_decl_names
    tab$postsim <- tab$postsim | ps_decl
  }
  return(tab)
}
