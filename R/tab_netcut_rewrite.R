#' Netcut enforcement, stage E2 (roadmap 6.5). The solver places every
#' element of a variable referenced with a lead or lag into the dense
#' border of the bordered matrix methods, so an inter-period link on an
#' element slice of a large variable (e.g. qo("capital",r,t+1)) borders
#' the whole variable. When a lead/lag reference fixes one or more
#' dimensions to quoted elements, the border contribution is reducible
#' mechanically: synthesize a proxy variable over the remaining
#' dimensions (NCV*), tie it to the slice with a linking equation
#' (E_NCV*), and move the lead/lag onto the proxy. References that run
#' over full sets are not reducible and are left to the .check_netcut
#' warning.
#'
#' @importFrom purrr map_chr
#'
#' @keywords internal
#' @noRd
.rewrite_tab_netcut <- function(tab,
                                call) {
  int_sets <- regmatches(tab, regexec(
    "^[Ss][Ee][Tt]\\s*\\(\\s*[Ii][Nn][Tt][Ee][Rr][Tt][Ee][Mm][Pp][Oo][Rr][Aa][Ll]\\s*\\)\\s*([A-Za-z_][A-Za-z0-9_]*)",
    tab
  ))
  int_sets <- toupper(purrr::map_chr(
    int_sets[lengths(int_sets) > 0L],
    2
  ))

  if (length(int_sets) %=% 0L) {
    return(tab)
  }

  vars <- .netcut_var_table(tab)

  if (nrow(vars) %=% 0L) {
    return(tab)
  }

  synth <- new.env(parent = emptyenv())
  synth$n <- 0L
  synth$tab <- tab
  synth$rewrites <- character(0)
  additions <- list()

  is_eq <- grepl("^[Ee][Qq][Uu][Aa][Tt][Ii][Oo][Nn][^A-Za-z0-9_]", tab)

  for (s in which(is_eq)) {
    refs <- .netcut_lagged_refs(tab[s], vars)
    if (nrow(refs) %=% 0L) {
      next
    }
    stmt <- tab[s]
    for (k in rev(seq_len(nrow(refs)))) {
      proxy <- .netcut_proxy(
        ref = refs[k, ],
        vars = vars,
        synth = synth
      )
      if (is.null(proxy)) {
        next
      }
      if (length(proxy$pre) > 0L) {
        at <- as.character(proxy$after)
        additions[[at]] <- c(additions[[at]], proxy$pre)
        synth$tab <- c(synth$tab, proxy$pre)
      }
      stmt <- paste0(
        substr(stmt, 1L, refs$start[k] - 1L),
        proxy$ref,
        substring(stmt, refs$end[k] + 1L)
      )
    }
    tab[s] <- stmt
  }

  if (length(additions) %=% 0L) {
    return(tab)
  }

  out <- vector("list", length(tab))
  for (s in seq_along(tab)) {
    out[[s]] <- c(tab[s], additions[[as.character(s)]])
  }

  proxy_summary <- synth$rewrites
  .cli_action(model_info$netcut_rewrite,
    action = c("inform", "inform"),
    call = call
  )

  tab <- unlist(out, use.names = FALSE)
  return(tab)
}
