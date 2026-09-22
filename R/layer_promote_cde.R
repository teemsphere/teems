#' @keywords internal
#' @noRd
.layer_promote_cde <- function(i_data, spec, call) {
  for (from in names(spec$cde$pairs)) {
    p <- spec$cde$pairs[[from]]
    to <- p$to
    nm <- toupper(names(i_data))
    i <- match(from, nm)
    a <- i_data[[i]]
    dn <- names(dimnames(a))
    if (is.null(dn) || !dn[[1]] %=% spec$cde$dim) {
      e_header <- from
      e_dim <- if (is.null(dn)) {
        "unnamed"
      } else {
        dn[[1]]
      }
      .cli_action(data_err$e_topp_dim,
        action = "abort",
        call = call
      )
    }
    if (!is.na(p$max)) {
      e_max <- signif(max(a, na.rm = TRUE), 4)
      if (e_max > p$max) {
        e_header <- from
        .cli_action(data_err$cde_range,
          action = c("abort", "inform"),
          call = call
        )
      }
    }
    class(a)[1] <- to
    i_data[[i]] <- a
    names(i_data)[i] <- to
    drop <- setdiff(which(toupper(names(i_data)) %in% to), i)
    if (length(drop) > 0L) {
      i_data <- i_data[-drop]
    }
  }
  return(i_data)
}
