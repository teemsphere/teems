#' @keywords internal
#' @noRd
.tab_subset_closure <- function(tab) {
  id <- "[A-Za-z_][A-Za-z0-9_]*"
  # the subset pairs are collected in an explicit environment so the
  # collector needs no super-assignment
  acc <- new.env(parent = emptyenv())
  acc$rel <- list()
  add <- function(acc, a, b) {
    acc$rel[[length(acc$rel) + 1L]] <- c(toupper(a), toupper(b))
    return(invisible(NULL))
  }
  op_re <- "\\+|-|&|(^|[^A-Za-z0-9_])[Uu][Nn][Ii][Oo][Nn]([^A-Za-z0-9_]|$)|(^|[^A-Za-z0-9_])[Ii][Nn][Tt][Ee][Rr][Ss][Ee][Cc][Tt]([^A-Za-z0-9_]|$)"
  for (st in tab) {
    m <- regmatches(st, regexec(
      paste0("^\\s*[Ss][Uu][Bb][Ss][Ee][Tt]\\s+(", id, ")\\s+[Ii][Ss]\\s+[Ss][Uu][Bb][Ss][Ee][Tt]\\s+[Oo][Ff]\\s+(", id, ")"),
      st
    ))[[1]]
    if (length(m) > 0L) {
      add(acc, m[2], m[3])
      next
    }
    if (!grepl("^\\s*[Ss][Ee][Tt]\\b", st)) {
      next
    }
    body <- sub("^\\s*[Ss][Ee][Tt]\\s*", "", st)
    body <- gsub("#[^#]*#", " ", body)
    body <- sub("^\\s*(\\([^)]*\\)\\s*)*", "", body)
    m <- regmatches(body, regexec(paste0("^\\s*(", id, ")\\s*=\\s*(.*)$"), body))[[1]]
    if (length(m) %=% 0L) {
      next
    }
    nm <- m[2]
    rhs <- trimws(m[3])
    sb <- regmatches(rhs, regexec(
      paste0("^\\(\\s*[Aa][Ll][Ll]\\s*,\\s*", id, "\\s*,\\s*(", id, ")"), rhs
    ))[[1]]
    if (length(sb) > 0L) {
      add(acc, nm, sb[2])
      next
    }
    if (grepl('[]["(){}]', rhs)) {
      next
    }
    ops <- toupper(trimws(regmatches(rhs, gregexpr(op_re, rhs))[[1]]))
    parts <- trimws(strsplit(rhs, op_re)[[1]])
    parts <- parts[nzchar(parts)]
    if (length(parts) %=% 0L || !all(grepl(paste0("^", id, "$"), parts))) {
      next
    }
    if (length(ops) %=% 0L) {
      add(acc, nm, parts[1])
      add(acc, parts[1], nm)
    } else if (all(ops %in% c("+", "UNION"))) {
      for (pt in parts) add(acc, pt, nm)
    } else if (all(ops %in% c("&", "INTERSECT"))) {
      for (pt in parts) add(acc, nm, pt)
    } else if (all(ops %=% "-")) {
      add(acc, nm, parts[1])
    }
  }
  sup <- list()
  for (r in acc$rel) sup[[r[1]]] <- union(sup[[r[1]]], r[2])
  repeat {
    changed <- FALSE
    for (a in names(sup)) {
      more <- unique(unlist(sup[sup[[a]]], use.names = FALSE))
      new <- setdiff(more, c(sup[[a]], a))
      if (length(new) > 0L) {
        sup[[a]] <- c(sup[[a]], new)
        changed <- TRUE
      }
    }
    if (!changed) {
      break
    }
  }
  return(sup)
}
