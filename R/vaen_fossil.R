#' @importFrom data.table data.table
#' @keywords internal
#' @noRd
.vaen_agg <- function(m,
                      set_mappings) {
  Value <- NULL

  dn <- lapply(dimnames(m), tolower)
  dt <- data.table::data.table(
    ACTS = rep(dn[[1]], times = length(dn[[2]])),
    REG = rep(dn[[2]], each = length(dn[[1]])),
    Value = as.vector(m)
  )
  dt <- .map_data(dt = dt, sets = set_mappings, col = c("ACTS", "REG"))
  dt <- dt[, list(Value = sum(Value)), keyby = c("ACTS", "REG")]
  return(dt)
}

#' @keywords internal
#' @noRd
.vaen_map_acts <- function(acts,
                           set_mappings) {
  tab <- set_mappings[["ACTS"]]
  mapped <- acts
  if (!is.null(tab)) {
    mapped <- tab[, 2][[1]][match(acts, tolower(tab[, 1][[1]]))]
  }
  return(mapped)
}

#' @importFrom data.table copy let setnames
#' @keywords internal
#' @noRd
.fossil_vaen <- function(agg_data,
                         i_data,
                         set_raw,
                         set_mappings,
                         metadata,
                         ndigits) {
  ACTS <- Value <- i.Value <- NULL

  flag <- intersect(c("ep", "e"), names(metadata)[vapply(metadata, isTRUE, logical(1))])
  needed <- c("EFVE", "SPLY", "EVFP", "VDFP", "VMFP")
  if (length(flag) %=% 0L || !all(needed %in% names(i_data)) ||
    !all(c("COME", "FUEL") %in% names(set_raw))) {
    return(agg_data)
  }

  raw <- lapply(i_data[c("EVFP", "VDFP", "VMFP", "SPLY")], \(arr) {
    arr <- unclass(arr)
    dimnames(arr) <- lapply(dimnames(arr), tolower)
    return(arr)
  })
  evfp <- raw$EVFP
  flows <- raw$VDFP + raw$VMFP
  sply <- raw$SPLY

  fixed <- intersect(layer_spec[[flag[1]]]$vocab$natural_resource, dimnames(evfp)[[1]])
  come <- intersect(set_raw$COME, dimnames(flows)[[1]])

  va <- apply(evfp, c(2, 3), sum)
  vaen <- va + apply(flows[come, , , drop = FALSE], c(2, 3), sum)
  vos <- va + apply(flows, c(2, 3), sum)
  nres <- apply(evfp[fixed, , , drop = FALSE], c(2, 3), sum)

  shares <- .vaen_agg(vos, set_mappings)
  data.table::setnames(shares, "Value", "vos")
  shares[, let(
    vaen = .vaen_agg(vaen, set_mappings)$Value,
    nres = .vaen_agg(nres, set_mappings)$Value
  )]

  mining <- .vaen_map_acts(rownames(sply), set_mappings)
  supply <- tapply(rowMeans(sply), mining, mean)
  fuel <- unique(.vaen_map_acts(set_raw$FUEL, set_mappings))

  efve <- data.table::copy(agg_data$EFVE)
  efve[ACTS %in% setdiff(fuel, names(supply)), let(Value = 1)]
  shares <- shares[ACTS %in% names(supply)]
  shares[, let(Value = supply[ACTS] / (vos / nres - vos / vaen))]
  shares[!(nres > 0 & vaen > 0 & is.finite(Value)), let(Value = 1)]
  efve[shares, let(Value = .round_digits(i.Value, ndigits)), on = c("ACTS", "REG")]

  agg_data$EFVE <- efve
  return(agg_data)
}
