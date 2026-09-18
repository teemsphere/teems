#' Index, element and mapping comparisons in an IF condition (GEMPACK
#' manual 11.4.11) rewritten onto `$POS` (11.5.6) so they ride the
#' expression-helper route:
#'   r EQ s        -> $POS(r) = $POS(s)            same set
#'   c EQ m        -> $POS(c) = $POS(m,COMM)       m over a subset of c's set
#'   r <> "usa"    -> $POS(r) <> $POS("usa",REG)   element against the index's set
#'   MAP(r) EQ b   -> $POS(MAP(r)) = $POS(b)       codomain against b's set
#'   t <= u        -> $POS(t) <= $POS(u)           ordered: intertemporal sets only
#' Every other pairing that involves an index, a quoted element or a
#' mapping expression is a named abort: before this the classifier let
#' them fall through to the arithmetic helper (`[r] - ["usa"]`), which
#' the solver evaluated as 0 - 0, so the condition held everywhere and
#' the run finished with wrong values and no message. Conditions with
#' neither operand of those kinds return unchanged.
#'
#' @keywords internal
#' @noRd
.if_index_cond <- function(cond_info,
                           quant,
                           q_idx,
                           synth,
                           if_cond,
                           call) {
  if (!cond_info$kind %in% c("expr", "cmp")) {
    return(cond_info)
  }
  lhs <- if (cond_info$kind %=% "cmp") {
    cond_info$ref
  } else {
    cond_info$lhs
  }
  rhs <- if (cond_info$kind %=% "cmp") {
    cond_info$num
  } else {
    cond_info$rhs
  }
  live <- !is.na(q_idx)
  idx_set <- stats::setNames(
    purrr::map_chr(quant[live], "set"),
    toupper(q_idx[live])
  )
  maps <- .tab_mappings(synth$tab)
  classify <- function(x) {
    x <- trimws(x)
    if (grepl('^"[^"]+"$', x)) {
      side <- list(kind = "elem", text = x)
      return(side)
    }
    if (grepl("^[A-Za-z_][A-Za-z0-9_]*$", x) && toupper(x) %in% names(idx_set)) {
      side <- list(kind = "index", text = x, set = idx_set[[toupper(x)]])
      return(side)
    }
    m <- regmatches(x, regexec(
      "^([A-Za-z_][A-Za-z0-9_]*)\\s*\\(\\s*([A-Za-z_][A-Za-z0-9_]*)\\s*\\)$", x
    ))[[1]]
    if (length(m) > 0L && toupper(m[2]) %in% names(maps) &&
      toupper(m[3]) %in% names(idx_set)) {
      side <- list(kind = "map", text = x, set = maps[[toupper(m[2])]][2])
      return(side)
    }
    side <- list(kind = "expr", text = x)
    return(side)
  }
  a <- classify(lhs)
  b <- classify(rhs)
  if (a$kind %=% "expr" && b$kind %=% "expr") {
    return(cond_info)
  }
  reject <- function(if_reason) {
    msg <- .cli_action(model_err$invalid_if_index_cond,
      action = c("abort", "inform"),
      call = call
    )
    return(msg)
  }
  if (a$kind %=% "expr" || b$kind %=% "expr") {
    reject(paste(
      "An index, quoted element or mapping expression can only be compared with",
      "another index, mapping expression or quoted element (GEMPACK manual 11.4.11);",
      "a data comparison takes a coefficient reference on both sides."
    ))
  }
  if (a$kind %=% "elem" && b$kind %=% "elem") {
    reject("Two quoted elements compared with each other is a constant condition (GEMPACK manual 11.4.11).")
  }
  # the set each side lies in; an element lies in the other side's set
  set_a <- if (a$kind %=% "elem") {
    b$set
  } else {
    a$set
  }
  set_b <- if (b$kind %=% "elem") {
    a$set
  } else {
    b$set
  }
  pos <- function(side, lift) {
    if (side$kind %=% "elem") {
      pos_expr <- sprintf("$POS(%s,%s)", side$text, lift)
      return(pos_expr)
    }
    if (toupper(lift) %=% toupper(side$set)) {
      pos_expr <- sprintf("$POS(%s)", side$text)
      return(pos_expr)
    }
    pos_expr <- sprintf("$POS(%s,%s)", side$text, lift)
    return(pos_expr)
  }
  if (toupper(set_a) %=% toupper(set_b)) {
    common <- set_a
  } else if (.tab_is_subset(set_a, set_b, synth)) {
    common <- set_b
  } else if (.tab_is_subset(set_b, set_a, synth)) {
    common <- set_a
  } else {
    reject(sprintf(paste(
      "The compared sets %s and %s are neither equal nor is one a declared subset",
      "of the other (GEMPACK manual 11.4.11.2)."
    ), set_a, set_b))
  }
  if (!cond_info$op %in% c("=", "<>") &&
    !toupper(common) %in% .tab_intertemporal_sets(synth$tab)) {
    reject(sprintf(paste(
      "Ordered comparisons (< <= > >=) of indices need an intertemporal set;",
      "%s is not one, so only EQ/NE apply (GEMPACK manual 11.4.11.2)."
    ), common))
  }
  cond <- list(kind = "expr", lhs = pos(a, common), op = cond_info$op, rhs = pos(b, common))
  return(cond)
}
