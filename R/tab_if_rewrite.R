#' GEMPACK IF in formulas (manual 11.4.5-11.4.7). An IF term entering a
#' Formula's right-hand side additively at the top level is rewritten
#' into sequential formulas the solver executes natively: a base
#' formula with the IF terms dropped, then one accumulate formula per
#' IF term whose domain is narrowed to where the condition holds, so
#' the value expression is only ever evaluated there (matching GEMPACK,
#' which does not evaluate the expression when the condition fails).
#' Conditions follow the manual's simple shapes:
#'   <index> in <set>     the index's quantifier is narrowed to
#'                        <set> & <range> (rule 4: <set> need not be a
#'                        subset of the current range)
#'   <index> = "element"  the quantifier is narrowed to a synthesized
#'                        singleton set "element" & <range>
#'   <coefref> <op> <c>   a conditional quantifier ':' is appended to
#'                        the statement's last quantifier
#'   <expr> <op> <expr>   a helper coefficient IFX<n> = <expr> [- <expr>]
#'                        is synthesized ahead of the statement and the
#'                        condition takes the <coefref> route above
#' An IF whose value itself carries top-level IF terms is rewritten
#' recursively on the narrowed statement (the GTAP-AEZ shapes: a
#' membership IF wrapping comparison IFs in a Formula, membership IFs
#' on a second index inside a membership branch of an Equation). A
#' membership set that is the quantifier's set or a declared subset
#' of it narrows to that set directly; otherwise an intersection set
#' is synthesized. A Formula whose right-hand side references its own
#' target (the AEZ calibration `ESUBVA = IF[.., ESUBVA] + ..`) reads
#' the pre-assignment values through a synthesized copy coefficient,
#' so the sequential rewrite never destroys what it still needs.
#' Compound conditions (AND/OR/NOT) and non-additive IF placement
#' abort.
#'
#' @keywords internal
#' @noRd
.rewrite_tab_if <- function(tab,
                            call) {
  if_pattern <- "(^|[^A-Za-z0-9_])[Ii][Ff]\\s*[][({]"
  has_if <- grepl(if_pattern, tab)
  if (!any(has_if)) {
    return(tab)
  }

  synth <- new.env(parent = emptyenv())
  synth$n <- 0L
  synth$tab <- tab

  out <- vector("list", length(tab))
  for (s in seq_along(tab)) {
    if (!has_if[s]) {
      out[[s]] <- tab[s]
      next
    }
    type <- tolower(sub("\\s.*$", "", tab[s]))
    if (type %=% "formula") {
      out[[s]] <- .rewrite_formula_if(
        stmt = tab[s],
        synth = synth,
        call = call
      )
    } else if (type %=% "equation") {
      out[[s]] <- .rewrite_equation_if(
        stmt = tab[s],
        synth = synth,
        call = call
      )
    } else {
      out[[s]] <- tab[s]
    }
  }
  unlist(out, use.names = FALSE)
}

#' Character-level bracket depth before each character; '(' '[' '{' are
#' interchangeable (manual 11.4.6). Quoted element literals are opaque.
#'
#' @keywords internal
#' @noRd
.tab_scan <- function(s) {
  chs <- strsplit(s, "")[[1]]
  qcum <- cumsum(chs == '"')
  in_quote <- qcum %% 2L == 1L | chs == '"'
  opens <- chs %in% c("(", "[", "{") & !in_quote
  closes <- chs %in% c(")", "]", "}") & !in_quote
  depth <- cumsum(opens) - cumsum(closes)
  list(
    chs = chs,
    in_quote = in_quote,
    depth_before = c(0L, depth[-length(chs)])
  )
}

#' Position of the bracket matching the opener at position i.
#'
#' @keywords internal
#' @noRd
.match_bracket <- function(s, i) {
  scan <- .tab_scan(s)
  target <- scan$depth_before[i] + 1L
  n <- length(scan$chs)
  j <- i + 1L
  while (j <= n) {
    if (scan$depth_before[j] == target &&
      scan$chs[j] %in% c(")", "]", "}") && !scan$in_quote[j]) {
      return(j)
    }
    j <- j + 1L
  }
  NA_integer_
}

#' Split an expression into its top-level additive terms.
#'
#' @keywords internal
#' @noRd
.split_tab_terms <- function(s) {
  scan <- .tab_scan(s)
  chs <- scan$chs
  n <- length(chs)
  is_pm <- chs %in% c("+", "-") & scan$depth_before == 0L & !scan$in_quote
  idx <- which(is_pm)
  # a +/- directly after an exponent marker continues a numeric literal
  expo <- idx > 2L & chs[pmax(idx - 1L, 1L)] %in% c("e", "E") &
    grepl("[0-9.]", chs[pmax(idx - 2L, 1L)])
  idx <- idx[!expo]
  first_char <- which(chs != " ")[1]
  lead_sign <- !is.na(first_char) && chs[first_char] %in% c("+", "-")
  idx <- idx[!idx %in% first_char]
  starts <- c(1L, idx)
  ends <- c(idx - 1L, n)
  body <- substring(s, starts, ends)
  sign <- c(
    if (lead_sign && chs[first_char] %=% "-") "-" else "+",
    chs[idx]
  )
  if (lead_sign) {
    body[1] <- sub("^\\s*[+-]", "", body[1])
  }
  body[-1] <- sub("^[+-]", "", body[-1])
  list(sign = sign, body = trimws(body))
}

#' Is this term exactly one IF call? Returns its condition and value
#' (split at the first top-level comma) or NULL.
#'
#' @keywords internal
#' @noRd
.parse_if_term <- function(body) {
  if (!grepl("^[Ii][Ff]\\s*[][({]", body)) {
    return(NULL)
  }
  open <- regexpr("[][({]", body)
  close <- .match_bracket(body, open)
  if (is.na(close) || nchar(trimws(substring(body, close + 1L))) > 0L) {
    return(NULL)
  }
  inner <- substr(body, open + 1L, close - 1L)
  scan <- .tab_scan(inner)
  comma <- which(scan$chs == "," & scan$depth_before == 0L & !scan$in_quote)
  if (length(comma) %=% 0L) {
    return(NULL)
  }
  list(
    cond = trimws(substr(inner, 1L, comma[1] - 1L)),
    value = trimws(substring(inner, comma[1] + 1L))
  )
}

#' Classify an IF condition into the supported shapes.
#'
#' @keywords internal
#' @noRd
.classify_if_cond <- function(cond) {
  m <- regmatches(cond, regexec(
    "^([A-Za-z_][A-Za-z0-9_]*)\\s+[Ii][Nn]\\s+([A-Za-z_][A-Za-z0-9_]*)$",
    cond
  ))[[1]]
  if (length(m) > 0L) {
    return(list(kind = "in_set", idx = m[2], set = m[3]))
  }
  m <- regmatches(cond, regexec(
    '^([A-Za-z_][A-Za-z0-9_]*)\\s*=\\s*"([^"]+)"$',
    cond
  ))[[1]]
  if (length(m) > 0L) {
    return(list(kind = "elem", idx = m[2], elem = m[3]))
  }
  ops <- c(
    eq = "=", ne = "<>", gt = ">", lt = "<", ge = ">=", le = "<=",
    "=" = "=", "<>" = "<>", ">" = ">", "<" = "<", ">=" = ">=", "<=" = "<="
  )
  m <- regmatches(cond, regexec(
    paste0(
      "^([A-Za-z_][A-Za-z0-9_]*(\\([^()]*\\))?)\\s*",
      "([Ee][Qq]|[Nn][Ee]|[Gg][Tt]|[Ll][Tt]|[Gg][Ee]|[Ll][Ee]|<=|>=|<>|=|<|>)\\s*",
      "([-+]?[0-9]*\\.?[0-9]+([eE][-+]?[0-9]+)?)$"
    ),
    cond
  ))[[1]]
  if (length(m) > 0L) {
    return(list(
      kind = "cmp",
      ref = gsub("\\s", "", m[2]),
      op = ops[[tolower(m[4])]],
      num = m[5]
    ))
  }
  # general comparison of two arithmetic expressions (manual 11.4.5:
  # "conditions must be logical expressions ... typically comparison
  # operators"): carried by a synthesized helper coefficient, see
  # .if_expr_helper
  sp <- .split_comparison(cond)
  if (!is.null(sp)) {
    return(list(kind = "expr", lhs = sp$lhs, op = sp$op, rhs = sp$rhs))
  }
  NULL
}

#' Split a condition at its top-level comparison operator. Symbol ops
#' (= <> < > <= >=) and word ops (eq ne lt gt le ge, identifier-
#' bounded); AND/OR/NOT compounds are not split (NULL). Returns
#' list(lhs, op, rhs) with op in symbol form, or NULL.
#'
#' @keywords internal
#' @noRd
.split_comparison <- function(cond) {
  scan <- .tab_scan(cond)
  chs <- scan$chs
  top <- scan$depth_before == 0L & !scan$in_quote
  n <- length(chs)
  if (grepl("(^|[^A-Za-z0-9_])(and|or|not)([^A-Za-z0-9_]|$)", cond, ignore.case = TRUE)) {
    return(NULL)
  }
  words <- c(eq = "=", ne = "<>", gt = ">", lt = "<", ge = ">=", le = "<=")
  is_id <- function(ch) grepl("[A-Za-z0-9_]", ch)
  i <- 1L
  while (i <= n) {
    if (top[i]) {
      c1 <- chs[i]
      c2 <- if (i < n) chs[i + 1L] else ""
      if (c1 %in% c("<", ">")) {
        len <- if (c2 %in% c("=", ">")) 2L else 1L
        return(list(
          lhs = trimws(substr(cond, 1L, i - 1L)),
          op = substr(cond, i, i + len - 1L),
          rhs = trimws(substring(cond, i + len))
        ))
      }
      if (c1 == "=") {
        return(list(
          lhs = trimws(substr(cond, 1L, i - 1L)),
          op = "=",
          rhs = trimws(substring(cond, i + 1L))
        ))
      }
      w <- tolower(paste0(c1, c2))
      if (w %in% names(words) &&
        (i == 1L || !is_id(chs[i - 1L])) &&
        (i + 1L >= n || !is_id(chs[i + 2L]))) {
        return(list(
          lhs = trimws(substr(cond, 1L, i - 1L)),
          op = words[[w]],
          rhs = trimws(substring(cond, i + 2L))
        ))
      }
    }
    i <- i + 1L
  }
  NULL
}

#' Names declared by statements of one type in the raw statement
#' vector (Variable/Coefficient ...): leading qualifier and quantifier
#' groups stripped, first identifier taken.
#'
#' @keywords internal
#' @noRd
.tab_declared_names <- function(tab, type) {
  stmts <- tab[grepl(paste0("^\\s*", type, "\\b"), tab, ignore.case = TRUE)]
  if (length(stmts) == 0L) {
    return(character(0))
  }
  x <- sub(paste0("^\\s*", type, "\\s*"), "", stmts, ignore.case = TRUE)
  x <- gsub("#[^#]*#", "", x)
  x <- gsub("\\(all\\s*,[^)]*\\)", "", x, ignore.case = TRUE)
  x <- gsub("^\\s*(\\([^)]*\\)\\s*)*", "", x)
  toupper(sub("^\\s*([A-Za-z_][A-Za-z0-9_]*).*$", "\\1", x))
}

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
  lhs <- if (cond_info$kind %=% "cmp") cond_info$ref else cond_info$lhs
  rhs <- if (cond_info$kind %=% "cmp") cond_info$num else cond_info$rhs
  live <- !is.na(q_idx)
  idx_set <- stats::setNames(
    purrr::map_chr(quant[live], "set"),
    toupper(q_idx[live])
  )
  maps <- .tab_mappings(synth$tab)
  classify <- function(x) {
    x <- trimws(x)
    if (grepl('^"[^"]+"$', x)) {
      return(list(kind = "elem", text = x))
    }
    if (grepl("^[A-Za-z_][A-Za-z0-9_]*$", x) && toupper(x) %in% names(idx_set)) {
      return(list(kind = "index", text = x, set = idx_set[[toupper(x)]]))
    }
    m <- regmatches(x, regexec(
      "^([A-Za-z_][A-Za-z0-9_]*)\\s*\\(\\s*([A-Za-z_][A-Za-z0-9_]*)\\s*\\)$", x
    ))[[1]]
    if (length(m) > 0L && toupper(m[2]) %in% names(maps) &&
      toupper(m[3]) %in% names(idx_set)) {
      return(list(kind = "map", text = x, set = maps[[toupper(m[2])]][2]))
    }
    list(kind = "expr", text = x)
  }
  a <- classify(lhs)
  b <- classify(rhs)
  if (a$kind %=% "expr" && b$kind %=% "expr") {
    return(cond_info)
  }
  reject <- function(if_reason) {
    .cli_action(model_err$invalid_if_index_cond,
      action = c("abort", "inform"),
      call = call
    )
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
  set_a <- if (a$kind %=% "elem") b$set else a$set
  set_b <- if (b$kind %=% "elem") a$set else b$set
  pos <- function(side, lift) {
    if (side$kind %=% "elem") {
      sprintf("$POS(%s,%s)", side$text, lift)
    } else if (toupper(lift) %=% toupper(side$set)) {
      sprintf("$POS(%s)", side$text)
    } else {
      sprintf("$POS(%s,%s)", side$text, lift)
    }
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
  list(kind = "expr", lhs = pos(a, common), op = cond_info$op, rhs = pos(b, common))
}

#' Declared set mappings: NAME -> c(domain, codomain), upper-cased.
#'
#' @keywords internal
#' @noRd
.tab_mappings <- function(tab) {
  stmts <- tab[grepl("^\\s*[Mm][Aa][Pp][Pp][Ii][Nn][Gg]\\b", tab)]
  out <- list()
  for (st in stmts) {
    body <- sub("^\\s*[Mm][Aa][Pp][Pp][Ii][Nn][Gg]\\s*", "", st)
    body <- gsub("#[^#]*#", " ", body)
    body <- sub("^\\s*(\\([^)]*\\)\\s*)*", "", body)
    m <- regmatches(body, regexec(
      "^\\s*([A-Za-z_][A-Za-z0-9_]*)\\s+[Ff][Rr][Oo][Mm]\\s+([A-Za-z_][A-Za-z0-9_]*)\\s+[Tt][Oo]\\s+([A-Za-z_][A-Za-z0-9_]*)",
      body
    ))[[1]]
    if (length(m) > 0L) {
      out[[toupper(m[2])]] <- toupper(c(m[3], m[4]))
    }
  }
  out
}

#' Names of the sets declared `Set (intertemporal)`, upper-cased.
#'
#' @keywords internal
#' @noRd
.tab_intertemporal_sets <- function(tab) {
  stmts <- tab[grepl("^\\s*[Ss][Ee][Tt]\\s*\\(\\s*[Ii][Nn][Tt][Ee][Rr][Tt][Ee][Mm][Pp][Oo][Rr][Aa][Ll]\\s*\\)", tab)]
  toupper(sub("^\\s*[Ss][Ee][Tt]\\s*\\([^)]*\\)\\s*([A-Za-z_][A-Za-z0-9_]*).*$", "\\1", stmts))
}

#' Names of the declared LINEAR variables (every Variable statement
#' whose qualifiers do not say levels), upper-cased.
#'
#' @keywords internal
#' @noRd
.tab_linear_variable_names <- function(tab) {
  stmts <- tab[grepl("^\\s*variable\\b", tab, ignore.case = TRUE)]
  if (length(stmts) == 0L) {
    return(character(0))
  }
  quals <- vapply(stmts, function(st) {
    body <- sub("^\\s*variable\\s*", "", st, ignore.case = TRUE)
    body <- gsub("#[^#]*#", "", body)
    paste(regmatches(body, gregexpr("^\\s*(\\([^)]*\\)\\s*)*", body))[[1]], collapse = "")
  }, character(1), USE.NAMES = FALSE)
  quals <- gsub("\\(all\\s*,[^)]*\\)", "", quals, ignore.case = TRUE)
  linear <- !grepl("levels", quals, ignore.case = TRUE)
  .tab_declared_names(stmts[linear], "variable")
}

#' Helper coefficient for an expression-valued IF condition
#'
#' `IF[<lhs> <op> <rhs>, value]` where a side is an arithmetic
#' expression (the LULC family's `THETAi(j,r)*YDONOFF(j,r) <= 0`) is
#' carried by a synthesized coefficient IFX<n>, quantified over the
#' host indices the expression uses, assigned `<lhs>` (numeric-
#' constant rhs) or `<lhs> - <rhs>` right before the host statement,
#' inheriting the host Formula's (initial)/(always) qualifier (an
#' Equation host gets the ALWAYS default, re-evaluated every step like
#' the equation itself). The condition then takes the existing
#' `<coefref> <op> <constant>` route. Zerodivide defaults active at
#' the host apply to the helper alike. Cached by expression and
#' index-set signature. Returns list(pre, cond_info) with cond_info of
#' kind "cmp".
#'
#' @keywords internal
#' @noRd
.if_expr_helper <- function(cond_info,
                            quant,
                            q_idx,
                            qual_groups,
                            synth,
                            if_cond,
                            stmt,
                            call) {
  expr <- if (grepl("^[-+]?([0-9]+\\.?[0-9]*|\\.[0-9]+)([eE][-+]?[0-9]+)?$", cond_info$rhs)) {
    num <- cond_info$rhs
    cond_info$lhs
  } else {
    num <- "0"
    paste0("[", cond_info$lhs, "] - [", cond_info$rhs, "]")
  }
  # linear variables cannot enter a condition (11.4.5/11.4.8); levels
  # variables can, and reach the solver as their paired value
  # coefficient. The helper is a Formula.
  var_names <- .tab_linear_variable_names(synth$tab)
  toks <- toupper(unique(regmatches(expr, gregexpr("[A-Za-z_][A-Za-z0-9_]*", expr))[[1]]))
  # a quantifier index shadows any variable of the same name inside
  # the statement (GTAP-RE's utility u against an index u)
  toks <- setdiff(toks, toupper(q_idx[!is.na(q_idx)]))
  if (any(toks %in% var_names)) {
    if_statement <- stmt
    bad_vars <- toks[toks %in% var_names]
    .cli_action(model_err$if_cond_variable,
      action = c("abort", "inform"),
      call = call
    )
  }
  live <- !is.na(q_idx)
  used <- vapply(q_idx[live], function(ix) {
    grepl(paste0("(^|[^A-Za-z0-9_])", ix, "([^A-Za-z0-9_]|$)"), expr, ignore.case = TRUE)
  }, logical(1))
  dims <- q_idx[live][used]
  sets <- purrr::map_chr(quant[live][used], "set")
  canon <- toupper(gsub("\\s", "", expr))
  for (k in seq_along(dims)) {
    canon <- gsub(
      paste0("(^|[^A-Za-z0-9_])", dims[k], "([^A-Za-z0-9_]|$)"),
      paste0("\\1<", sets[k], ">\\2"),
      canon,
      ignore.case = TRUE
    )
  }
  key <- paste0(
    "EXPR|", canon, "|", paste(toupper(sets), collapse = ","), "|",
    paste(qual_groups, collapse = "")
  )
  nm <- synth[[key]]
  pre <- character(0)
  if (is.null(nm)) {
    nm <- .synth_expr_name(synth)
    synth[[key]] <- nm
    quants <- paste0(sprintf("(all,%s,%s)", dims, sets), collapse = "")
    dimargs <- if (length(dims) > 0L) paste0("(", paste(dims, collapse = ","), ")") else ""
    qual <- if (length(qual_groups) > 0L) paste0(paste(qual_groups, collapse = ""), " ") else ""
    pre <- c(
      sprintf(
        "Coefficient %s%s%s # if-rewrite condition %s #",
        quants, if (nzchar(quants)) " " else "", paste0(nm, dimargs), if_cond
      ),
      sprintf("Formula %s%s%s%s = %s", qual, quants, if (nzchar(quants)) " " else "", paste0(nm, dimargs), expr)
    )
    pre <- gsub("\\s{2,}", " ", pre)
  }
  ref <- if (length(dims) > 0L) paste0(nm, "(", paste(dims, collapse = ","), ")") else nm
  list(
    pre = pre,
    cond_info = list(kind = "cmp", ref = ref, op = cond_info$op, num = num)
  )
}

#' A fresh helper-coefficient name unused anywhere in the model.
#'
#' @keywords internal
#' @noRd
.synth_expr_name <- function(synth) {
  if (is.null(synth$nx)) {
    synth$nx <- 0L
  }
  repeat {
    synth$nx <- synth$nx + 1L
    nm <- paste0("IFX", synth$nx)
    hit <- paste0("(^|[^A-Za-z0-9_])", nm, "([^A-Za-z0-9_]|$)")
    if (!any(grepl(hit, synth$tab, ignore.case = TRUE))) {
      return(nm)
    }
  }
}

#' A fresh set name unused anywhere in the model.
#'
#' @keywords internal
#' @noRd
.synth_set_name <- function(synth) {
  repeat {
    synth$n <- synth$n + 1L
    nm <- paste0("IFS", synth$n)
    hit <- paste0("(^|[^A-Za-z0-9_])", nm, "([^A-Za-z0-9_]|$)")
    if (!any(grepl(hit, synth$tab, ignore.case = TRUE))) {
      return(nm)
    }
  }
}

#' A fresh coefficient name unused anywhere in the model.
#'
#' @keywords internal
#' @noRd
.synth_coeff_name <- function(synth) {
  if (is.null(synth$nc)) {
    synth$nc <- 0L
  }
  repeat {
    synth$nc <- synth$nc + 1L
    nm <- paste0("IFC", synth$nc)
    hit <- paste0("(^|[^A-Za-z0-9_])", nm, "([^A-Za-z0-9_]|$)")
    if (!any(grepl(hit, synth$tab, ignore.case = TRUE))) {
      return(nm)
    }
  }
}

#' The set a membership condition narrows a quantifier to: the operand
#' itself when it is the range set or a declared subset of it (no
#' synthesized set, and the narrowed statement keeps the declared
#' set relations the solver needs), otherwise a synthesized
#' intersection set (cached by operand and range). Returns
#' list(pre, name).
#'
#' @keywords internal
#' @noRd
.synth_intersect_set <- function(operand, range_set, synth) {
  if (grepl("^[A-Za-z_][A-Za-z0-9_]*$", operand) &&
    .tab_is_subset(operand, range_set, synth)) {
    return(list(pre = character(0), name = operand))
  }
  key <- toupper(paste0(operand, "&", range_set))
  nm <- synth[[key]]
  pre <- character(0)
  if (is.null(nm)) {
    nm <- .synth_set_name(synth)
    synth[[key]] <- nm
    pre <- sprintf(
      "Set %s # if-rewrite %s intersect %s # = %s & %s",
      nm, operand, range_set, operand, range_set
    )
  }
  list(pre = pre, name = nm)
}

#' A 0/1 indicator coefficient for a data-comparison IF condition in an
#' Equation (the shipped MAKESUNIT/E_qca adaptation shape). Cached by
#' the condition with index names canonicalized to their sets, so the
#' same condition in another equation reuses one indicator. Returns
#' list(pre, ref) where ref uses the current statement's index names.
#'
#' @keywords internal
#' @noRd
.if_indicator <- function(cond_info,
                          quant,
                          q_idx,
                          synth,
                          if_cond,
                          call) {
  argstr <- sub("^[A-Za-z_][A-Za-z0-9_]*", "", cond_info$ref)
  args <- if (nchar(argstr) > 0L) {
    trimws(strsplit(gsub("[()]", "", argstr), ",")[[1]])
  } else {
    character(0)
  }
  is_idx <- !grepl('^"', args)
  at <- match(tolower(args[is_idx]), tolower(q_idx))
  if (length(args[is_idx]) %=% 0L || anyNA(at)) {
    # scalar conditions have no quantifier to carry the comparison
    .cli_action(model_err$invalid_if_cond,
      action = c("abort", "inform"),
      call = call
    )
  }
  dims <- args[is_idx]
  sets <- purrr::map_chr(quant[at], "set")
  canon <- args
  canon[is_idx] <- toupper(sets)
  key <- paste0(
    "CMP|", toupper(sub("\\(.*$", "", cond_info$ref)),
    "(", paste(canon, collapse = ","), ")|", cond_info$op, "|", cond_info$num
  )
  ind <- synth[[key]]
  pre <- character(0)
  if (is.null(ind)) {
    ind <- .synth_coeff_name(synth)
    synth[[key]] <- ind
    quants <- sprintf("(all,%s,%s)", dims, sets)
    n <- length(quants)
    quants_cond <- c(
      quants[-n],
      sprintf(
        "(all,%s,%s: %s %s %s)",
        dims[n], sets[n], cond_info$ref, cond_info$op, cond_info$num
      )
    )
    dimargs <- paste(dims, collapse = ",")
    pre <- c(
      sprintf(
        "Coefficient %s %s(%s) # if-rewrite indicator %s #",
        paste0(quants, collapse = ""), ind, dimargs, if_cond
      ),
      sprintf(
        "Formula %s %s(%s) = 0",
        paste0(quants, collapse = ""), ind, dimargs
      ),
      sprintf(
        "Formula %s %s(%s) = 1",
        paste0(quants_cond, collapse = ""), ind, dimargs
      )
    )
  }
  list(pre = pre, ref = sprintf("%s(%s)", ind, paste(dims, collapse = ",")))
}

#' Fresh equation names for a domain split (A, B, C, ... suffixes).
#'
#' @keywords internal
#' @noRd
.synth_eq_names <- function(name, synth, n = 2L) {
  purrr::map_chr(LETTERS[seq_len(n)], function(suffix) {
    nm <- paste0(name, suffix)
    hit <- paste0("(^|[^A-Za-z0-9_])", nm, "([^A-Za-z0-9_]|$)")
    while (any(grepl(hit, synth$tab, ignore.case = TRUE))) {
      nm <- paste0(nm, suffix)
      hit <- paste0("(^|[^A-Za-z0-9_])", nm, "([^A-Za-z0-9_]|$)")
    }
    nm
  })
}

#' Rewrite one Equation statement. Data-comparison IF terms become 0/1
#' indicator coefficients distributed over the value's top-level terms
#' (the value must already be valid over the full domain, as in
#' GEMPACK, where only IN conditions relax index checking). Set-
#' membership or element IF terms split the equation into
#' complementary-domain equations (manual 11.4.7 rule 2: inside the
#' value the index is deemed to range over the condition's set, so the
#' value may reference arrays declared only there and must not be
#' evaluated elsewhere): one equation per membership term over
#' operand & range (that term kept, the other membership terms
#' dropped) plus one over range - (S1 + S2 + ...) with every membership
#' term dropped. All membership terms must condition the same index;
#' the '+' union is disjointness-checked by the solver (manual 10.1.1),
#' so overlapping conditions (the GTAPv7 partitions "domestic"/
#' "imported", ACTS/"hhld"/"govt"/"invt" never overlap) fail loudly
#' rather than dropping a term. Returns the replacement statements.
#'
#' @keywords internal
#' @noRd
.rewrite_equation_if <- function(stmt,
                                 synth,
                                 call) {
  rest <- trimws(sub("^\\s*[Ee][Qq][Uu][Aa][Tt][Ii][Oo][Nn]\\s*", "", stmt))

  qual <- ""
  if (startsWith(rest, "(")) {
    close <- .match_bracket(rest, 1L)
    qual <- paste0(substr(rest, 1L, close), " ")
    rest <- trimws(substring(rest, close + 1L))
  }
  name <- sub("^([A-Za-z_][A-Za-z0-9_]*).*$", "\\1", rest)
  rest <- trimws(sub("^[A-Za-z_][A-Za-z0-9_]*", "", rest))

  label <- ""
  if (startsWith(rest, "#")) {
    m <- regexpr("^#[^#]*#", rest)
    label <- paste0(substr(rest, 1L, attr(m, "match.length")), " ")
    rest <- trimws(substring(rest, attr(m, "match.length") + 1L))
  }

  groups <- character(0)
  repeat {
    rest <- sub("^\\s+", "", rest)
    if (!startsWith(rest, "(")) break
    close <- .match_bracket(rest, 1L)
    groups <- c(groups, substr(rest, 1L, close))
    rest <- substring(rest, close + 1L)
  }

  quant <- lapply(groups, function(g) {
    inner <- trimws(substr(g, 2L, nchar(g) - 1L))
    m <- regmatches(inner, regexec(
      "^[Aa][Ll][Ll]\\s*,\\s*([A-Za-z_][A-Za-z0-9_]*)\\s*,\\s*([A-Za-z_][A-Za-z0-9_]*)\\s*(:.*)?$",
      inner
    ))[[1]]
    if (length(m) %=% 0L) {
      return(list(is_quant = FALSE, text = g))
    }
    list(
      is_quant = TRUE, text = g, idx = m[2], set = m[3],
      cond = nchar(m[4]) > 0L
    )
  })
  q_idx <- purrr::map_chr(quant, function(q) {
    if (isTRUE(q$is_quant)) q$idx else NA_character_
  })

  scan <- .tab_scan(rest)
  eq_pos <- which(scan$chs == "=" & scan$depth_before == 0L & !scan$in_quote)
  sides <- list(
    trimws(substr(rest, 1L, eq_pos[1] - 1L)),
    trimws(substring(rest, eq_pos[1] + 1L))
  )

  if_pattern <- "(^|[^A-Za-z0-9_])[Ii][Ff]\\s*[][({]"
  pre <- character(0)
  membership <- list()

  # pass 1: indicators for data comparisons; collect membership terms
  side_terms <- vector("list", 2L)
  for (h in 1:2) {
    terms <- .split_tab_terms(sides[[h]])
    parsed <- lapply(terms$body, .parse_if_term)
    is_if <- !purrr::map_lgl(parsed, is.null)
    if (any(grepl(if_pattern, terms$body[!is_if]))) {
      if_statement <- stmt
      .cli_action(model_err$invalid_if_placement,
        action = c("abort", "inform"),
        call = call
      )
    }
    chunks <- paste(terms$sign, terms$body)
    for (k in which(is_if)) {
      if_cond <- parsed[[k]]$cond
      cond_info <- .classify_if_cond(if_cond)
      if (is.null(cond_info)) {
        .cli_action(model_err$invalid_if_cond,
          action = c("abort", "inform"),
          call = call
        )
      }
      cond_info <- .if_index_cond(cond_info, quant, q_idx, synth, if_cond, call)
      if (cond_info$kind %=% "expr") {
        # an Equation host: the helper is an ordinary (always) Formula
        hx <- .if_expr_helper(cond_info, quant, q_idx, character(0), synth, if_cond, stmt, call)
        pre <- c(pre, hx$pre)
        cond_info <- hx$cond_info
      }
      if (cond_info$kind %=% "cmp") {
        ind <- .if_indicator(cond_info, quant, q_idx, synth, if_cond, call)
        pre <- c(pre, ind$pre)
        vt <- .split_tab_terms(parsed[[k]]$value)
        chunks[k] <- paste(
          ifelse(vt$sign == terms$sign[k], "+", "-"),
          paste0(ind$ref, " * ", vt$body),
          collapse = " "
        )
      } else {
        membership[[length(membership) + 1L]] <- list(
          side = h, term = k, cond_info = cond_info,
          sign = terms$sign[k], value = parsed[[k]]$value,
          if_cond = if_cond
        )
      }
    }
    side_terms[[h]] <- chunks
  }

  assemble <- function(side_chunks, quants, eq_name) {
    header <- paste0(purrr::map_chr(quants, "text"), collapse = "")
    lhs <- sub("^\\+\\s*", "", paste(side_chunks[[1]], collapse = " "))
    rhs <- sub("^\\+\\s*", "", paste(side_chunks[[2]], collapse = " "))
    if (lhs %=% "") lhs <- "0"
    if (rhs %=% "") rhs <- "0"
    paste0(
      "Equation ", qual, eq_name, " ", label, header, " ",
      lhs, " = ", rhs
    )
  }

  if (length(membership) %=% 0L) {
    return(c(pre, assemble(side_terms, quant, name)))
  }

  # pass 2: split the equation on the membership conditions' domains
  at <- NA_integer_
  inter_names <- character(0)
  for (m in membership) {
    cond_info <- m$cond_info
    at_m <- match(tolower(cond_info$idx), tolower(q_idx))
    if (is.na(at_m) || isTRUE(quant[[at_m]]$cond) ||
      (cond_info$kind %=% "in_set" &&
        toupper(cond_info$set) %in% toupper(q_idx[!is.na(q_idx)]))) {
      if_cond <- m$if_cond
      .cli_action(model_err$invalid_if_cond,
        action = c("abort", "inform"),
        call = call
      )
    }
    if (!is.na(at) && at_m != at) {
      # membership terms on different indices would need a nested
      # (product) split; no measured model demand
      if_statement <- stmt
      .cli_action(model_err$invalid_if_multi,
        action = c("abort", "inform"),
        call = call
      )
    }
    at <- at_m
    range_set <- quant[[at]]$set
    operand <- if (cond_info$kind %=% "in_set") {
      cond_info$set
    } else {
      paste0('"', cond_info$elem, '"')
    }
    inter <- .synth_intersect_set(operand, range_set, synth)
    pre <- c(pre, inter$pre)
    inter_names <- c(inter_names, inter$name)
  }
  idx_name <- membership[[1]]$cond_info$idx
  range_set <- quant[[at]]$set

  # remainder domain: range - S1 (one term) or range - (S1 + S2 + ...);
  # the '+' is disjointness-checked at set resolution
  union_expr <- if (length(inter_names) %=% 1L) {
    inter_names
  } else {
    paste0("(", paste(inter_names, collapse = " + "), ")")
  }
  comp_key <- toupper(paste0(range_set, "-", union_expr))
  comp <- synth[[comp_key]]
  if (is.null(comp)) {
    comp <- .synth_set_name(synth)
    synth[[comp_key]] <- comp
    pre <- c(pre, sprintf(
      "Set %s # if-rewrite %s minus %s # = %s - %s",
      comp, range_set, union_expr, range_set, union_expr
    ))
  }

  n_m <- length(membership)
  eq_names <- .synth_eq_names(name, synth, n_m + 1L)

  # equation k keeps membership term k as [value] and drops the others;
  # the remainder equation drops all of them
  split_eq <- function(keep) {
    chunks <- side_terms
    drop <- list(integer(0), integer(0))
    for (j in seq_len(n_m)) {
      m <- membership[[j]]
      if (identical(j, keep)) {
        # a value carrying IF terms of its own is spread over the side
        # so the recursive pass sees them at the top level
        chunks[[m$side]][m$term] <- if (grepl(if_pattern, m$value)) {
          .distribute_terms(m$sign, m$value)
        } else {
          paste(m$sign, paste0("[", m$value, "]"))
        }
      } else {
        drop[[m$side]] <- c(drop[[m$side]], m$term)
      }
    }
    for (h in 1:2) {
      if (length(drop[[h]]) > 0L) {
        chunks[[h]] <- chunks[[h]][-drop[[h]]]
      }
    }
    chunks
  }

  out <- character(0)
  for (k in seq_len(n_m)) {
    q_k <- quant
    q_k[[at]]$text <- sprintf("(all,%s,%s)", idx_name, inter_names[k])
    eq_k <- assemble(split_eq(k), q_k, eq_names[k])
    if (grepl(if_pattern, membership[[k]]$value)) {
      # nested IF terms (membership on another index, or comparisons)
      # partition or gate the split equation in turn
      eq_k <- .rewrite_equation_if(eq_k, synth, call)
    }
    out <- c(out, eq_k)
  }
  q_out <- quant
  q_out[[at]]$text <- sprintf("(all,%s,%s)", idx_name, comp)
  out <- c(out, assemble(split_eq(0L), q_out, eq_names[n_m + 1L]))

  c(pre, out)
}
#' Rewrite one Formula statement (see .rewrite_tab_if). `depth` > 0
#' marks a recursive pass over an accumulate statement, whose leading
#' right-hand-side term is the target itself.
#'
#' @keywords internal
#' @noRd
.rewrite_formula_if <- function(stmt,
                                synth,
                                call,
                                depth = 0L) {
  body <- sub("^\\s*[Ff][Oo][Rr][Mm][Uu][Ll][Aa]\\s*", "", stmt)

  label <- ""
  if (startsWith(trimws(body), "#")) {
    close <- regexpr("#[^#]*#", body)
    label <- paste0(substr(body, close, close + attr(close, "match.length") - 1L), " ")
    body <- substring(body, close + attr(close, "match.length"))
  }

  groups <- character(0)
  repeat {
    body <- sub("^\\s+", "", body)
    if (!startsWith(body, "(")) break
    close <- .match_bracket(body, 1L)
    groups <- c(groups, substr(body, 1L, close))
    body <- substring(body, close + 1L)
  }

  quant <- lapply(groups, function(g) {
    inner <- trimws(substr(g, 2L, nchar(g) - 1L))
    m <- regmatches(inner, regexec(
      "^[Aa][Ll][Ll]\\s*,\\s*([A-Za-z_][A-Za-z0-9_]*)\\s*,\\s*([A-Za-z_][A-Za-z0-9_]*)\\s*(:.*)?$",
      inner
    ))[[1]]
    if (length(m) %=% 0L) {
      return(list(is_quant = FALSE, text = g))
    }
    list(
      is_quant = TRUE, text = g, idx = m[2], set = m[3],
      cond = nchar(m[4]) > 0L, cond_text = m[4]
    )
  })

  scan <- .tab_scan(body)
  eq_pos <- which(scan$chs == "=" & scan$depth_before == 0L & !scan$in_quote)
  lhs <- trimws(substr(body, 1L, eq_pos[1] - 1L))
  rhs <- trimws(substring(body, eq_pos[1] + 1L))

  q_idx <- purrr::map_chr(quant, function(q) {
    if (isTRUE(q$is_quant)) q$idx else NA_character_
  })
  qual_groups <- purrr::map_chr(quant[!purrr::map_lgl(quant, "is_quant")], "text")
  pre <- character(0)

  # a self-referencing right-hand side reads the values as they stand
  # before this formula; the sequential rewrite would overwrite them
  # with the base assignment, so it reads a copy instead (rewritten
  # once, at the top: below, the leading term is the accumulator)
  lhs_sym <- sub("^\\s*([A-Za-z_][A-Za-z0-9_]*).*$", "\\1", lhs)
  if (depth %=% 0L && .tab_mentions(rhs, lhs_sym)) {
    cp <- .if_self_copy(lhs, lhs_sym, quant, qual_groups, synth)
    pre <- c(pre, cp$pre)
    rhs <- .tab_subst_symbol(rhs, lhs_sym, cp$name)
  }

  if_pattern <- "(^|[^A-Za-z0-9_])[Ii][Ff]\\s*[][({]"
  terms <- .split_tab_terms(rhs)
  parsed <- lapply(terms$body, .parse_if_term)
  is_if <- !purrr::map_lgl(parsed, is.null)

  if (any(grepl(if_pattern, terms$body[!is_if]))) {
    if_statement <- stmt
    .cli_action(model_err$invalid_if_placement,
      action = c("abort", "inform"),
      call = call
    )
  }

  base_rhs <- paste(
    ifelse(terms$sign[!is_if] == "-", "- ", ""),
    terms$body[!is_if],
    sep = "",
    collapse = " + "
  )
  base_rhs <- gsub("+ - ", "- ", base_rhs, fixed = TRUE)
  if (base_rhs %=% "") {
    base_rhs <- "0"
  }
  header <- paste0(label, paste0(purrr::map_chr(quant, "text"), collapse = ""))
  # a recursive pass whose only non-IF term is the accumulator has
  # nothing to assign
  statements <- if (depth > 0L && base_rhs %=% lhs) {
    character(0)
  } else {
    paste("Formula", header, lhs, "=", base_rhs)
  }

  narrow <- function(cond_info, if_cond) {
    at <- match(tolower(cond_info$idx), tolower(q_idx))
    if (is.na(at)) {
      .cli_action(model_err$invalid_if_cond,
        action = c("abort", "inform"),
        call = call
      )
    }
    range_set <- quant[[at]]$set
    operand <- if (cond_info$kind %=% "in_set") {
      cond_info$set
    } else {
      paste0('"', cond_info$elem, '"')
    }
    inter <- .synth_intersect_set(operand, range_set, synth)
    pre <<- c(pre, inter$pre)
    q2 <- quant
    # a condition already on the quantifier stays with it
    q2[[at]]$text <- sprintf("(all,%s,%s%s)", cond_info$idx, inter$name, quant[[at]]$cond_text)
    q2
  }

  for (k in which(is_if)) {
    if_cond <- parsed[[k]]$cond
    cond_info <- .classify_if_cond(if_cond)
    if (is.null(cond_info)) {
      .cli_action(model_err$invalid_if_cond,
        action = c("abort", "inform"),
        call = call
      )
    }
    cond_info <- .if_index_cond(cond_info, quant, q_idx, synth, if_cond, call)
    if (cond_info$kind %=% "expr") {
      hx <- .if_expr_helper(cond_info, quant, q_idx, qual_groups, synth, if_cond, stmt, call)
      pre <- c(pre, hx$pre)
      cond_info <- hx$cond_info
    }
    if (cond_info$kind %in% c("in_set", "elem")) {
      if (cond_info$kind %=% "in_set" &&
        toupper(cond_info$set) %in% toupper(q_idx[!is.na(q_idx)])) {
        # an index, not a set, on the right of IN
        .cli_action(model_err$invalid_if_cond,
          action = c("abort", "inform"),
          call = call
        )
      }
      q2 <- narrow(cond_info, if_cond)
      header2 <- paste0(label, paste0(purrr::map_chr(q2, "text"), collapse = ""))
    } else {
      # the comparison rides the last quantifier still free of a
      # condition (conditions are evaluated per tuple, so any position
      # serves)
      free <- !is.na(q_idx) & !purrr::map_lgl(quant, function(q) isTRUE(q$cond))
      last_q <- max(which(free), -Inf)
      if (is.infinite(last_q)) {
        .cli_action(model_err$invalid_if_cond,
          action = c("abort", "inform"),
          call = call
        )
      }
      q2 <- quant
      q2[[last_q]]$text <- sprintf(
        "(all,%s,%s: %s %s %s)",
        quant[[last_q]]$idx, quant[[last_q]]$set,
        cond_info$ref, cond_info$op, cond_info$num
      )
      header2 <- paste0(label, paste0(purrr::map_chr(q2, "text"), collapse = ""))
    }
    value <- parsed[[k]]$value
    if (grepl(if_pattern, value)) {
      # IF terms inside the value: narrow, then rewrite the accumulate
      # statement in turn
      statements <- c(statements, .rewrite_formula_if(
        paste("Formula", header2, lhs, "=", lhs, .distribute_terms(terms$sign[k], value)),
        synth, call,
        depth = depth + 1L
      ))
    } else {
      statements <- c(statements, paste(
        "Formula", header2, lhs, "=",
        lhs, terms$sign[k], paste0("[", value, "]")
      ))
    }
  }

  c(pre, statements)
}

#' Spread a signed IF value over its top-level terms: "+ a - b" for
#' sign "+" and value "a - b", the signs flipped for "-".
#'
#' @keywords internal
#' @noRd
.distribute_terms <- function(sign, value) {
  vt <- .split_tab_terms(value)
  paste(ifelse(vt$sign == sign, "+", "-"), vt$body, collapse = " ")
}

#' Does `text` reference symbol `sym` (identifier-bounded, any case)?
#'
#' @keywords internal
#' @noRd
.tab_mentions <- function(text, sym) {
  grepl(paste0("(?<![A-Za-z0-9_])", sym, "(?![A-Za-z0-9_])"), text, ignore.case = TRUE, perl = TRUE)
}

#' Replace every identifier-bounded occurrence of `sym` in `text`.
#'
#' @keywords internal
#' @noRd
.tab_subst_symbol <- function(text, sym, new) {
  gsub(paste0("(?<![A-Za-z0-9_])", sym, "(?![A-Za-z0-9_])"), new, text, ignore.case = TRUE, perl = TRUE)
}

#' The copy coefficient a self-referencing Formula reads from:
#' declared like its target (the target's Coefficient statement, else
#' the host's quantifiers), assigned the target's values right before
#' the host under the host's (initial)/(always) qualifier. Returns
#' list(pre, name).
#'
#' @keywords internal
#' @noRd
.if_self_copy <- function(lhs, sym, quant, qual_groups, synth) {
  decl <- .tab_coeff_decl(synth$tab, sym)
  if (is.null(decl)) {
    qs <- quant[purrr::map_lgl(quant, "is_quant")]
    decl <- list(
      quants = paste0(purrr::map_chr(qs, function(q) sprintf("(all,%s,%s)", q$idx, q$set)), collapse = ""),
      args = gsub("\\s", "", sub("^\\s*[A-Za-z_][A-Za-z0-9_]*\\s*", "", lhs))
    )
  }
  nm <- .synth_copy_name(synth)
  qual <- if (length(qual_groups) > 0L) paste0(paste(qual_groups, collapse = ""), " ") else ""
  sep <- if (nzchar(decl$quants)) " " else ""
  list(
    pre = c(
      sprintf("Coefficient %s%s%s # if-rewrite copy of %s #", decl$quants, sep, paste0(nm, decl$args), sym),
      sprintf("Formula %s%s%s%s = %s", qual, decl$quants, sep, paste0(nm, decl$args), paste0(sym, decl$args))
    ),
    name = nm
  )
}

#' Quantifiers and argument list of a Coefficient declaration, from
#' the raw statements: list(quants, args) or NULL when `sym` is not a
#' declared coefficient.
#'
#' @keywords internal
#' @noRd
.tab_coeff_decl <- function(tab, sym) {
  stmts <- tab[grepl("^\\s*[Cc][Oo][Ee][Ff][Ff][Ii][Cc][Ii][Ee][Nn][Tt]\\b", tab)]
  for (st in stmts) {
    body <- sub("^\\s*[Cc][Oo][Ee][Ff][Ff][Ii][Cc][Ii][Ee][Nn][Tt]\\s*", "", st)
    body <- gsub("#[^#]*#", " ", body)
    groups <- character(0)
    repeat {
      body <- sub("^\\s+", "", body)
      if (!startsWith(body, "(")) break
      close <- .match_bracket(body, 1L)
      if (is.na(close)) break
      groups <- c(groups, substr(body, 1L, close))
      body <- substring(body, close + 1L)
    }
    nm <- sub("^\\s*([A-Za-z_][A-Za-z0-9_]*).*$", "\\1", body)
    if (!toupper(nm) %=% toupper(sym)) next
    rest <- sub("^\\s*[A-Za-z_][A-Za-z0-9_]*\\s*", "", body)
    args <- ""
    if (startsWith(rest, "(")) {
      close <- .match_bracket(rest, 1L)
      if (!is.na(close)) args <- gsub("\\s", "", substr(rest, 1L, close))
    }
    quants <- groups[grepl("^\\(\\s*[Aa][Ll][Ll]\\s*,", groups)]
    return(list(quants = paste0(gsub("\\s", "", quants), collapse = ""), args = args))
  }
  NULL
}

#' A fresh copy-coefficient name unused anywhere in the model.
#'
#' @keywords internal
#' @noRd
.synth_copy_name <- function(synth) {
  if (is.null(synth$nt)) {
    synth$nt <- 0L
  }
  repeat {
    synth$nt <- synth$nt + 1L
    nm <- paste0("IFT", synth$nt)
    hit <- paste0("(^|[^A-Za-z0-9_])", nm, "([^A-Za-z0-9_]|$)")
    if (!any(grepl(hit, synth$tab, ignore.case = TRUE))) {
      return(nm)
    }
  }
}

#' Is `sub` the set `sup` or a declared subset of it? The relations
#' come from the raw statements (cached on `synth`): `Subset A is
#' subset of B`, and the definitions TABLO itself treats as declaring
#' them (manual 10.1): `Set X = A + B` (A, B in X), `Set X = A - B`
#' (X in A), `Set X = A & B` (X in A and B), `Set X = A` (both ways),
#' `Set X = (all,i,S: ...)` (X in S); transitively closed. Anything
#' else infers nothing, so a miss only costs a synthesized set.
#'
#' @keywords internal
#' @noRd
.tab_is_subset <- function(sub, sup, synth) {
  if (toupper(sub) %=% toupper(sup)) {
    return(TRUE)
  }
  if (is.null(synth$subsets)) {
    synth$subsets <- .tab_subset_closure(synth$tab)
  }
  toupper(sup) %in% synth$subsets[[toupper(sub)]]
}

#' @keywords internal
#' @noRd
.tab_subset_closure <- function(tab) {
  id <- "[A-Za-z_][A-Za-z0-9_]*"
  rel <- list()
  add <- function(a, b) {
    rel[[length(rel) + 1L]] <<- c(toupper(a), toupper(b))
  }
  op_re <- "\\+|-|&|(^|[^A-Za-z0-9_])[Uu][Nn][Ii][Oo][Nn]([^A-Za-z0-9_]|$)|(^|[^A-Za-z0-9_])[Ii][Nn][Tt][Ee][Rr][Ss][Ee][Cc][Tt]([^A-Za-z0-9_]|$)"
  for (st in tab) {
    m <- regmatches(st, regexec(
      paste0("^\\s*[Ss][Uu][Bb][Ss][Ee][Tt]\\s+(", id, ")\\s+[Ii][Ss]\\s+[Ss][Uu][Bb][Ss][Ee][Tt]\\s+[Oo][Ff]\\s+(", id, ")"),
      st
    ))[[1]]
    if (length(m) > 0L) {
      add(m[2], m[3])
      next
    }
    if (!grepl("^\\s*[Ss][Ee][Tt]\\b", st)) next
    body <- sub("^\\s*[Ss][Ee][Tt]\\s*", "", st)
    body <- gsub("#[^#]*#", " ", body)
    body <- sub("^\\s*(\\([^)]*\\)\\s*)*", "", body)
    m <- regmatches(body, regexec(paste0("^\\s*(", id, ")\\s*=\\s*(.*)$"), body))[[1]]
    if (length(m) %=% 0L) next
    nm <- m[2]
    rhs <- trimws(m[3])
    sb <- regmatches(rhs, regexec(
      paste0("^\\(\\s*[Aa][Ll][Ll]\\s*,\\s*", id, "\\s*,\\s*(", id, ")"), rhs
    ))[[1]]
    if (length(sb) > 0L) {
      add(nm, sb[2])
      next
    }
    if (grepl('[]["(){}]', rhs)) next
    ops <- toupper(trimws(regmatches(rhs, gregexpr(op_re, rhs))[[1]]))
    parts <- trimws(strsplit(rhs, op_re)[[1]])
    parts <- parts[nzchar(parts)]
    if (length(parts) %=% 0L || !all(grepl(paste0("^", id, "$"), parts))) next
    if (length(ops) %=% 0L) {
      add(nm, parts[1])
      add(parts[1], nm)
    } else if (all(ops %in% c("+", "UNION"))) {
      for (pt in parts) add(pt, nm)
    } else if (all(ops %in% c("&", "INTERSECT"))) {
      for (pt in parts) add(nm, pt)
    } else if (all(ops %=% "-")) {
      add(nm, parts[1])
    }
  }
  sup <- list()
  for (r in rel) sup[[r[1]]] <- union(sup[[r[1]]], r[2])
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
    if (!changed) break
  }
  sup
}
