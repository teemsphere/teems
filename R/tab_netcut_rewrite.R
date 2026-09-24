#' @importFrom purrr map_chr
#' @keywords internal
#' @noRd
.rewrite_tab_netcut <- function(tab,
                                call) {
  int_sets <- regmatches(tab, regexec(
    "^[Ss][Ee][Tt]\\s*\\(\\s*[Ii][Nn][Tt][Ee][Rr][Tt][Ee][Mm][Pp][Oo][Rr][Aa][Ll]\\s*\\)\\s*([A-Za-z_][A-Za-z0-9_@]*)",
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

  is_eq <- grepl("^[Ee][Qq][Uu][Aa][Tt][Ii][Oo][Nn][^A-Za-z0-9_@]", tab)

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
