#' @importFrom stats setNames
#' @keywords internal
#' @noRd
.layer_cde <- function(i_data, spec, topp, call) {
  nm <- toupper(names(i_data))
  weight <- Reduce(`+`, lapply(spec$cde$weight, \(h) {
    w <- unclass(i_data[[match(h, nm)]])
    dimnames(w) <- lapply(dimnames(w), tolower)
    w
  }))
  energy <- setdiff(tolower(dimnames(weight)[[1]]), topp)
  node <- topp[!topp %in% tolower(dimnames(weight)[[1]])]

  for (header in names(spec$cde$pars)) {
    p <- spec$cde$pars[[header]]
    i <- match(header, nm)
    a <- i_data[[i]]
    dn <- names(dimnames(a))
    if (is.null(dn) || !dn[[1]] %=% "COMM") {
      e_header <- header
      e_dim <- if (is.null(dn)) "unnamed" else dn[[1]]
      .cli_action(data_err$cde_dim, action = "abort", call = call)
    }
    if (!is.na(p$max)) {
      e_max <- signif(max(a, na.rm = TRUE), 4)
      if (e_max > p$max) {
        e_header <- header
        .cli_action(data_err$cde_range, action = c("abort", "inform"), call = call)
      }
    }
    source <- unclass(a)
    dimnames(source) <- lapply(dimnames(source), tolower)
    w <- weight[energy, , drop = FALSE]
    num <- colSums(w * source[energy, , drop = FALSE])
    den <- colSums(w)
    eny <- ifelse(den > 0, num / den, colMeans(source[energy, , drop = FALSE]))
    rebuilt <- rbind(eny, source[setdiff(topp, node), , drop = FALSE])
    dimnames(rebuilt) <- stats::setNames(
      list(c(node, setdiff(topp, node)), dimnames(source)[[2]]),
      c(spec$cde$dim, dn[[2]])
    )
    rebuilt <- rebuilt[topp, , drop = FALSE]
    class(rebuilt) <- class(a)
    i_data[[i]] <- rebuilt
  }
  drop <- which(nm %in% spec$cde$drop)
  if (length(drop) > 0L) {
    i_data <- i_data[-drop]
  }
  return(i_data)
}
