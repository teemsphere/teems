#' @keywords internal
#' @noRd
.sbx_ctx <- function(model, coeff_data, mappings, set_raw, limit_row) {
  ctx <- new.env(parent = emptyenv())
  ctx$model <- model
  ctx$coeff_data <- coeff_data
  ctx$mappings <- mappings
  ctx$set_raw <- set_raw
  ctx$limit_row <- limit_row
  ctx$cache <- list()
  lhs <- rep(NA_character_, nrow(model))
  is_f <- model$type %in% "Formula" & !is.na(model$comp1)
  lhs[is_f] <- tolower(trimws(sub("[[({].*$", "", model$comp1[is_f])))
  ctx$lhs <- lhs
  ctx$zdiv <- .sbx_zdiv(limit_row, ctx)
  return(ctx)
}

#' @keywords internal
#' @noRd
.sbx_zdiv <- function(row, ctx) {
  model <- ctx$model
  prior <- seq_len(min(row, nrow(model) + 1L) - 1L)
  zd <- 0
  for (z in prior[tolower(model$type[prior]) %in% "zerodivide"]) {
    txt <- model$tab[z]
    if (!grepl("\\bdefault\\b", txt, ignore.case = TRUE)) {
      next
    }
    v <- trimws(sub("^.*\\bdefault\\s+([^;]*).*$", "\\1", txt, ignore.case = TRUE))
    num <- suppressWarnings(as.numeric(v))
    zd <- if (!is.na(num)) {
      num
    } else {
      outer <- ctx$limit_row
      ctx$limit_row <- z - 1L
      val <- .sbx_coef(v, ctx)
      ctx$limit_row <- outer
      as.vector(val)[1]
    }
  }
  return(zd)
}

#' @keywords internal
#' @noRd
.sbx_coef <- function(name, ctx) {
  key <- tolower(name)
  model <- ctx$model
  upto <- min(ctx$limit_row, nrow(model))
  ckey <- paste0(key, "@", upto)
  if (!is.null(ctx$cache[[ckey]])) {
    return(ctx$cache[[ckey]])
  }
  ci <- which(model$type %in% "Coefficient" & tolower(model$name) == key)
  if (length(ci) == 0L) {
    .sbx_fail(.sbx_reason("unknown", name))
  }
  ci <- ci[[1]]
  sets <- model$ls_upper_idx[[ci]]
  if (length(sets) == 1L && is.na(sets)) {
    sets <- character(0)
  }
  ele <- lapply(sets, .sbx_set_ele, ctx = ctx)
  arr <- if (length(sets) > 0L) {
    array(0, dim = lengths(ele), dimnames = ele)
  } else {
    0
  }
  rows <- seq_len(upto)
  src <- rows[(model$type[rows] %in% "Read" & tolower(model$name[rows]) == key) |
    (!is.na(ctx$lhs[rows]) & ctx$lhs[rows] == key)]
  if (length(src) == 0L) {
    .sbx_fail(.sbx_reason("no_source", name))
  }
  for (r in src) {
    if (model$type[r] == "Read") {
      arr <- .sbx_read(arr, r, name, ctx)
    } else {
      arr <- .sbx_formula(arr, r, ctx)
    }
  }
  ctx$cache[[ckey]] <- arr
  return(arr)
}

#' @importFrom stats complete.cases
#' @keywords internal
#' @noRd
.sbx_read <- function(arr, r, name, ctx) {
  q <- ctx$model$qualifier_list[r]
  if (!is.na(q) && grepl("by_elements", q, ignore.case = TRUE)) {
    .sbx_fail(.sbx_reason("unsupported", name))
  }
  hdr <- ctx$model$header[r]
  hit <- match(toupper(hdr), toupper(names(ctx$coeff_data)))
  if (is.na(hit)) {
    .sbx_fail(.sbx_reason("no_data", name, hdr))
  }
  dt <- ctx$coeff_data[[hit]]
  dn <- dimnames(arr)
  cols <- setdiff(names(dt), "Value")
  if (length(dn) == 0L) {
    return(dt$Value[1])
  }
  decl <- toupper(ctx$model$ls_upper_idx[[which(ctx$model$type %in% "Coefficient" &
    tolower(ctx$model$name) == tolower(name))[1]]])
  slot <- rep(NA_integer_, length(cols))
  for (k in seq_along(cols)) {
    free <- setdiff(which(decl == toupper(sub("\\.[0-9]+$", "", cols[k]))), slot)
    if (length(free) > 0L) {
      slot[k] <- free[1]
    }
  }
  if (anyNA(slot)) {
    if (length(cols) != length(dn)) {
      .sbx_fail(.sbx_reason("args", name))
    }
    slot <- seq_along(cols)
  }
  pos <- matrix(NA_integer_, nrow(dt), length(dn))
  for (k in seq_along(cols)) {
    pos[, slot[k]] <- match(tolower(dt[[cols[k]]]), dn[[slot[k]]])
  }
  ok <- stats::complete.cases(pos[, slot, drop = FALSE])
  pos <- pos[ok, , drop = FALSE]
  val <- dt$Value[ok]
  for (d in setdiff(seq_along(dn), slot)) {
    n <- length(dn[[d]])
    pos <- pos[rep(seq_len(nrow(pos)), times = n), , drop = FALSE]
    pos[, d] <- rep(seq_len(n), each = length(val))
    val <- rep(val, times = n)
  }
  arr[pos] <- val
  return(arr)
}

#' @keywords internal
#' @noRd
.sbx_quantifiers <- function(statement) {
  body <- sub("^\\s*[A-Za-z]+\\s*", "", statement)
  body <- gsub("#[^#]*#", " ", body)
  quants <- list()
  repeat {
    body <- sub("^\\s+", "", body)
    if (!startsWith(body, "(")) {
      break
    }
    close <- .match_bracket(body, 1L)
    if (is.na(close)) {
      .sbx_fail(.sbx_reason("unsupported", statement))
    }
    g <- substr(body, 2L, close - 1L)
    body <- substring(body, close + 1L)
    m <- regmatches(g, regexec("^\\s*[Aa][Ll][Ll]\\s*,\\s*([A-Za-z_@][A-Za-z0-9_@]*)\\s*,\\s*([A-Za-z_@][A-Za-z0-9_@]*)\\s*(:(.*))?$", g))[[1]]
    if (length(m) == 0L) {
      next
    }
    cond <- if (nzchar(m[5])) {
      .sbx_parse(m[5])
    } else {
      NULL
    }
    quants[[length(quants) + 1L]] <- list(idx = m[2], set = m[3], cond = cond)
  }
  return(quants)
}

#' @keywords internal
#' @noRd
.sbx_formula <- function(arr, r, ctx) {
  model <- ctx$model
  outer <- list(limit_row = ctx$limit_row, zdiv = ctx$zdiv)
  on.exit({
    ctx$limit_row <- outer$limit_row
    ctx$zdiv <- outer$zdiv
  })
  ctx$zdiv <- .sbx_zdiv(r, ctx)
  ctx$limit_row <- r - 1L
  quants <- .sbx_quantifiers(model$tab[r])
  lhs <- .sbx_parse(model$comp1[r])
  rhs <- .sbx_parse(model$comp2[r])
  binds <- list()
  for (q in quants) {
    binds[[q$idx]] <- list(set = q$set, ele = .sbx_set_ele(q$set, ctx))
  }
  qidx <- names(binds)
  qele <- lapply(binds, `[[`, "ele")
  val <- .sbx_num(.sbx_eval(rhs, binds, ctx))
  if (length(setdiff(val$idx, qidx)) > 0L) {
    .sbx_fail(.sbx_reason("unsupported", model$tab[r]))
  }
  v <- .sbx_expand(val, qidx, qele)
  keep <- rep(TRUE, length(v))
  for (q in quants) {
    if (!is.null(q$cond)) {
      cv <- .sbx_num(.sbx_eval(q$cond, binds, ctx))
      keep <- keep & .sbx_expand(cv, qidx, qele) != 0
    }
  }
  dn <- dimnames(arr)
  if (length(dn) == 0L) {
    if (length(qidx) > 0L || length(v) != 1L) {
      .sbx_fail(.sbx_reason("unsupported", model$tab[r]))
    }
    return(v)
  }
  if (length(lhs$args) != length(dn)) {
    .sbx_fail(.sbx_reason("args", lhs$name))
  }
  grid <- if (length(qidx) > 0L) {
    expand.grid(qele, KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
  } else {
    data.frame(row.names = 1L)
  }
  pos <- do.call(cbind, lapply(seq_along(lhs$args), \(k) {
    a <- lhs$args[[k]]
    e <- if (a$t == "str") {
      rep(a$v, nrow(grid))
    } else if (a$t == "id" && is.null(a$args) && a$name %in% qidx) {
      tolower(grid[[a$name]])
    } else {
      .sbx_fail(.sbx_reason("unsupported", model$tab[r]))
    }
    match(e, dn[[k]])
  }))
  if (anyNA(pos[keep, , drop = FALSE])) {
    .sbx_fail(.sbx_reason("element", lhs$name))
  }
  arr[pos[keep, , drop = FALSE]] <- v[keep]
  return(arr)
}

#' @importFrom stats setNames
#' @keywords internal
#' @noRd
.sbx_mapping <- function(name, ctx) {
  model <- ctx$model
  mi <- which(model$type %in% "Mapping" & tolower(model$name) == tolower(name))
  if (length(mi) == 0L) {
    return(NULL)
  }
  key <- paste0("map:", tolower(name))
  if (!is.null(ctx$cache[[key]])) {
    return(ctx$cache[[key]])
  }
  dom <- model$comp1[mi[1]]
  cod <- model$comp2[mi[1]]
  rd <- which(model$type %in% "Read" & tolower(model$name) == tolower(name) &
    grepl("by_elements", model$qualifier_list, ignore.case = TRUE))
  if (length(rd) == 0L) {
    .sbx_fail(.sbx_reason("unsupported", name))
  }
  raw <- ctx$set_raw
  hit <- match(toupper(model$header[rd[1]]), toupper(names(raw)))
  if (is.na(hit)) {
    .sbx_fail(.sbx_reason("no_data", name, model$header[rd[1]]))
  }
  vals <- tolower(raw[[hit]])
  dom_map <- ctx$mappings[[dom]]
  cod_map <- ctx$mappings[[cod]]
  if (is.null(dom_map) || is.null(cod_map)) {
    .sbx_fail("", defer = TRUE)
  }
  dom_hdr <- model$header[which(model$type %in% "Set" & tolower(model$name) == tolower(dom))[1]]
  dh <- match(toupper(dom_hdr), toupper(names(raw)))
  dom_orig <- if (!is.na(dh)) {
    tolower(raw[[dh]])
  } else {
    tolower(unique(dom_map$origin))
  }
  if (length(vals) != length(dom_orig)) {
    .sbx_fail(.sbx_reason("no_data", name, model$header[rd[1]]))
  }
  dom_agg <- tolower(dom_map$mapping)[match(dom_orig, tolower(dom_map$origin))]
  val_agg <- tolower(cod_map$mapping)[match(vals, tolower(cod_map$origin))]
  ok <- !is.na(dom_agg) & !duplicated(dom_agg)
  out <- stats::setNames(val_agg[ok], dom_agg[ok])
  ctx$cache[[key]] <- out
  return(out)
}
