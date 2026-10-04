#' @keywords internal
#' @noRd
.sbx_fail <- function(reason, defer = FALSE) {
  cls <- if (defer) {
    c("sbx_defer", "error", "condition")
  } else {
    c("sbx_error", "error", "condition")
  }
  stop(structure(class = cls, list(message = reason, call = NULL)))
}

#' @keywords internal
#' @noRd
.sbx_val <- function(v, idx = character(0), ele = list()) {
  val <- list(v = v, idx = idx, ele = ele)
  return(val)
}

#' @keywords internal
#' @noRd
.sbx_expand <- function(x, idx, ele) {
  if (identical(x$idx, idx)) {
    return(x$v)
  }
  miss <- setdiff(idx, x$idx)
  n_miss <- prod(lengths(ele[miss]))
  v <- rep(x$v, times = n_miss)
  cur <- c(x$idx, miss)
  if (length(cur) > 1L) {
    v <- as.vector(aperm(array(v, lengths(ele[cur])), match(idx, cur)))
  }
  return(v)
}

#' @keywords internal
#' @noRd
.sbx_join <- function(...) {
  xs <- list(...)
  idx <- unique(unlist(lapply(xs, `[[`, "idx")))
  ele <- list()
  for (x in xs) {
    ele[x$idx] <- x$ele
  }
  ele <- ele[idx]
  vs <- lapply(xs, .sbx_expand, idx = idx, ele = ele)
  joined <- list(vs = vs, idx = idx, ele = ele)
  return(joined)
}

#' @keywords internal
#' @noRd
.sbx_num <- function(x) {
  if (is.character(x$v)) {
    .sbx_fail(.sbx_reason("char_value"))
  }
  return(x)
}

#' @keywords internal
#' @noRd
.sbx_reason <- function(key, ...) {
  reason <- sprintf(deploy_err$set_builder_reason[[key]], ...)
  return(reason)
}

#' @keywords internal
#' @noRd
.sbx_eval <- function(node, binds, ctx) {
  switch(node$t,
    num = .sbx_val(node$v),
    str = .sbx_val(node$v),
    un = .sbx_eval_un(node, binds, ctx),
    bin = .sbx_eval_bin(node, binds, ctx),
    agg = .sbx_eval_agg(node, binds, ctx),
    `if` = {
      j <- .sbx_join(.sbx_num(.sbx_eval(node$cond, binds, ctx)), .sbx_num(.sbx_eval(node$body, binds, ctx)))
      .sbx_val(ifelse(j$vs[[1]] != 0, j$vs[[2]], 0), j$idx, j$ele)
    },
    fun = .sbx_eval_fun(node, binds, ctx),
    pos = .sbx_eval_pos(node, binds, ctx),
    id = .sbx_eval_id(node, binds, ctx)
  )
}

#' @keywords internal
#' @noRd
.sbx_eval_un <- function(node, binds, ctx) {
  x <- .sbx_num(.sbx_eval(node$x, binds, ctx))
  x$v <- if (node$op == "neg") {
    -x$v
  } else {
    as.numeric(x$v == 0)
  }
  return(x)
}

#' @keywords internal
#' @noRd
.sbx_eval_bin <- function(node, binds, ctx) {
  a <- .sbx_eval(node$a, binds, ctx)
  b <- .sbx_eval(node$b, binds, ctx)
  j <- .sbx_join(a, b)
  va <- j$vs[[1]]
  vb <- j$vs[[2]]
  cmp <- node$op %in% c("eq", "ne", "lt", "gt", "le", "ge")
  if (is.character(va) || is.character(vb)) {
    if (!node$op %in% c("eq", "ne")) {
      .sbx_fail(.sbx_reason("char_value"))
    }
    same <- tolower(as.character(va)) == tolower(as.character(vb))
    v <- as.numeric(if (node$op == "eq") same else !same)
    val <- .sbx_val(v, j$idx, j$ele)
    return(val)
  }
  v <- switch(node$op,
    `+` = va + vb,
    `-` = va - vb,
    `*` = va * vb,
    `/` = ifelse(vb == 0, ctx$zdiv, va / ifelse(vb == 0, 1, vb)),
    `^` = va^vb,
    and = as.numeric(va != 0 & vb != 0),
    or = as.numeric(va != 0 | vb != 0),
    as.numeric(.sb_op_test(va, node$op, vb))
  )
  if (cmp) {
    v[is.na(v)] <- 0
  }
  val <- .sbx_val(v, j$idx, j$ele)
  return(val)
}

#' @importFrom stats setNames
#' @keywords internal
#' @noRd
.sbx_eval_agg <- function(node, binds, ctx) {
  ele <- .sbx_set_ele(node$set, ctx)
  inner <- binds
  inner[[node$idx]] <- list(set = node$set, ele = ele)
  body <- .sbx_num(.sbx_eval(node$body, inner, ctx))
  loop <- .sbx_val(ele, node$idx, stats::setNames(list(ele), node$idx))
  parts <- list(body, loop)
  if (!is.null(node$cond)) {
    parts <- c(parts, list(.sbx_num(.sbx_eval(node$cond, inner, ctx))))
  }
  j <- do.call(.sbx_join, parts)
  v <- j$vs[[1]]
  if (!is.null(node$cond)) {
    keep <- j$vs[[3]] != 0
  } else {
    keep <- rep(TRUE, length(v))
  }
  others <- setdiff(j$idx, node$idx)
  perm <- match(c(others, node$idx), j$idx)
  dims <- lengths(j$ele)
  n_other <- prod(lengths(j$ele[others]))
  arr_v <- matrix(as.vector(aperm(array(v, dims), perm)), nrow = n_other)
  arr_k <- matrix(as.vector(aperm(array(keep, dims), perm)), nrow = n_other)
  if (node$op == "sum") {
    out <- rowSums(ifelse(arr_k, arr_v, 0))
    val <- .sbx_val(as.vector(out), others, j$ele[others])
    return(val)
  }
  out <- vapply(seq_len(n_other), \(r) {
    x <- arr_v[r, arr_k[r, ]]
    switch(node$op,
      prod = prod(x),
      maxs = if (length(x)) max(x) else 0,
      mins = if (length(x)) min(x) else 0
    )
  }, numeric(1))
  val <- .sbx_val(out, others, j$ele[others])
  return(val)
}

#' @importFrom stats dnorm pnorm dlnorm plnorm
#' @keywords internal
#' @noRd
.sbx_eval_fun <- function(node, binds, ctx) {
  if (node$name %=% "random") {
    .sbx_fail(.sbx_reason("random"))
  }
  args <- lapply(node$args, \(a) .sbx_num(.sbx_eval(a, binds, ctx)))
  if (length(args) == 0L) {
    .sbx_fail(.sbx_reason("unsupported", node$name))
  }
  j <- do.call(.sbx_join, args)
  x <- j$vs[[1]]
  v <- switch(node$name,
    abs = abs(x),
    max = do.call(pmax, j$vs),
    min = do.call(pmin, j$vs),
    sqrt = sqrt(x),
    exp = exp(x),
    loge = log(x),
    log10 = log10(x),
    id01 = ifelse(x == 0, 1, x),
    id0v = ifelse(x == 0, j$vs[[2]], x),
    round = sign(x) * floor(abs(x) + 0.5),
    trunc0 = trunc(x),
    truncb = floor(x),
    normal = stats::dnorm(x),
    cumnormal = stats::pnorm(x),
    lognormal = ifelse(x > 0, stats::dlnorm(pmax(x, .Machine$double.xmin)), 0),
    cumlognormal = ifelse(x > 0, stats::plnorm(pmax(x, .Machine$double.xmin)), 0),
    gperf = 2 * stats::pnorm(x * sqrt(2)) - 1,
    gperfc = 2 * stats::pnorm(-x * sqrt(2))
  )
  val <- .sbx_val(v, j$idx, j$ele)
  return(val)
}

#' @keywords internal
#' @noRd
.sbx_eval_pos <- function(node, binds, ctx) {
  if (length(node$args) == 0L || length(node$args) > 2L) {
    .sbx_fail(.sbx_reason("unsupported", "$POS"))
  }
  x <- .sbx_eval(node$args[[1]], binds, ctx)
  if (length(node$args) == 2L) {
    set_node <- node$args[[2]]
    if (set_node$t != "id" || !is.null(set_node$args)) {
      .sbx_fail(.sbx_reason("unsupported", "$POS"))
    }
    ref <- .sbx_set_ele(set_node$name, ctx)
  } else {
    arg <- node$args[[1]]
    if (arg$t != "id" || is.null(binds[[arg$name]])) {
      .sbx_fail(.sbx_reason("unsupported", "$POS"))
    }
    ref <- binds[[arg$name]]$ele
  }
  x$v <- as.numeric(match(tolower(x$v), ref))
  x$v[is.na(x$v)] <- 0
  return(x)
}

#' @importFrom stats setNames
#' @keywords internal
#' @noRd
.sbx_eval_id <- function(node, binds, ctx) {
  nm <- node$name
  if (is.null(node$args)) {
    b <- binds[[nm]]
    if (!is.null(b)) {
      val <- .sbx_val(b$ele, nm, stats::setNames(list(b$ele), nm))
      return(val)
    }
    arr <- .sbx_coef(nm, ctx)
    if (length(dim(arr)) > 0L) {
      .sbx_fail(.sbx_reason("args", nm))
    }
    val <- .sbx_val(as.vector(arr))
    return(val)
  }
  args <- lapply(node$args, \(a) .sbx_eval(a, binds, ctx))
  map <- .sbx_mapping(nm, ctx)
  if (!is.null(map)) {
    if (length(args) != 1L) {
      .sbx_fail(.sbx_reason("args", nm))
    }
    x <- args[[1]]
    x$v <- unname(map[tolower(x$v)])
    x$v[is.na(x$v)] <- ""
    return(x)
  }
  arr <- .sbx_coef(nm, ctx)
  dn <- dimnames(arr)
  if (length(dn) != length(args)) {
    .sbx_fail(.sbx_reason("args", nm))
  }
  j <- do.call(.sbx_join, args)
  pos <- do.call(cbind, lapply(seq_along(j$vs), \(k) match(tolower(j$vs[[k]]), dn[[k]])))
  if (anyNA(pos)) {
    .sbx_fail(.sbx_reason("element", nm))
  }
  v <- if (length(dn) == 1L) {
    as.vector(arr)[pos[, 1]]
  } else {
    arr[pos]
  }
  val <- .sbx_val(v, j$idx, j$ele)
  return(val)
}

#' @keywords internal
#' @noRd
.sbx_set_ele <- function(name, ctx) {
  m <- ctx$mappings[[name]]
  if (is.null(m)) {
    hit <- match(tolower(name), tolower(names(ctx$mappings)))
    if (!is.na(hit)) {
      m <- ctx$mappings[[hit]]
    }
  }
  if (is.null(m)) {
    known <- tolower(name) %in% tolower(ctx$model$name[ctx$model$type == "Set"])
    if (!known) {
      .sbx_fail(.sbx_reason("unknown", name))
    }
    ele <- .if_rewrite_elements(ctx$model, name)
    if (is.null(ele)) {
      .sbx_fail("", defer = TRUE)
    }
    ele <- tolower(ele)
    return(ele)
  }
  ele <- tolower(unique(m$mapping))
  return(ele)
}
