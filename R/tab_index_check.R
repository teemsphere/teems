# Argument-domain check for quantified references (GEMPACK manual
# 10.1.2): an index that ranges over set S may stand at an argument
# position of a coefficient or variable declared over set D only when
# S is D itself or a declared (or implied) subset of D. TABLO refuses
# the TAB otherwise; the solver used to bind the index by its position
# within S and silently address the wrong elements of D (GTAP-AEZ
# scoping, 2026-09: a hand-listed LANDACTS without "Subset LANDACTS is
# subset of ACTS" made the split factor-demand rows write into the
# wrong activities and left the system structurally singular).
#
# Lexical pass over the raw statement extracts: declarations give each
# argument position its set from the declaration's own quantifiers;
# statement quantifiers and SUM indices give idx -> set for the
# executable statements; every `name(args)` reference to a declared
# coefficient/variable with bare-index arguments is checked. Non-bare
# arguments (quoted elements, mapped indices, lead/lag offsets) and
# unknown names are skipped, so the check never rejects a form it
# cannot read.

#' @keywords internal
#' @noRd
.set_superset_closure <- function(set_extract) {
  # closure[[D]] = every set that is a (transitive) subset of D
  nm <- tolower(set_extract$name)
  direct <- lapply(set_extract$subsets, function(s) {
    if (is.null(s) || all(is.na(s))) character() else tolower(s[!is.na(s)])
  })
  names(direct) <- nm
  closure <- direct
  for (d in nm) {
    seen <- character()
    queue <- direct[[d]]
    while (length(queue) > 0L) {
      s <- queue[[1]]
      queue <- queue[-1]
      if (s %in% seen) next
      seen <- c(seen, s)
      queue <- c(queue, direct[[s]] %|||% character())
    }
    closure[[d]] <- seen
  }
  closure
}

# "(all,i,SET[:cond])" quantifiers plus "sum{i,SET[:cond], ...}" /
# "sum(i,SET, ...)" scopes of one statement text: named character
# vector idx (lower case) -> set (as written). An index bound to two
# different sets in the same statement is dropped (unreadable scope).
#' @keywords internal
#' @noRd
.stmt_index_sets <- function(text) {
  pairs <- regmatches(text, gregexpr(
    "(\\(\\s*all|\\bsum\\s*[{(])\\s*,?\\s*([A-Za-z_][A-Za-z0-9_]*)\\s*,\\s*([A-Za-z_][A-Za-z0-9_]*)",
    text, ignore.case = TRUE, perl = TRUE
  ))[[1]]
  if (length(pairs) == 0L) {
    return(character())
  }
  parts <- regmatches(pairs, gregexpr("[A-Za-z_][A-Za-z0-9_]*", pairs))
  idx <- tolower(vapply(parts, function(p) p[[length(p) - 1L]], character(1)))
  set <- vapply(parts, function(p) p[[length(p)]], character(1))
  out <- character()
  for (k in seq_along(idx)) {
    if (idx[[k]] %in% names(out)) {
      prev <- out[[idx[[k]]]]
      if (!is.na(prev) && tolower(prev) != tolower(set[[k]])) {
        out[[idx[[k]]]] <- NA_character_
      }
      next
    }
    out[[idx[[k]]]] <- set[[k]]
  }
  out[!is.na(out)]
}

#' @keywords internal
#' @noRd
.strip_tab_labels <- function(text) {
  gsub("#[^#]*#", " ", text)
}

# declared set per argument position of every Coefficient/Variable
# declaration: named list, tolower(name) -> character vector of sets
#' @keywords internal
#' @noRd
.declared_arg_sets <- function(extract) {
  decl_rows <- which(tolower(extract$type) %in% c("coefficient", "variable"))
  out <- list()
  for (n in decl_rows) {
    text <- .strip_tab_labels(extract$remainder[[n]])
    idx_sets <- .stmt_index_sets(text)
    # the declared reference: last "name(args)" outside the quantifier groups
    body <- gsub("\\(\\s*all\\s*,[^)]*\\)", " ", text, ignore.case = TRUE)
    body <- gsub("\\([^()]*=[^()]*\\)|\\(\\s*(parameter|integer|real|levels|linear|change|percent_change|non_parameter|initial|always|ge|gt|le|lt)\\b[^()]*\\)", " ", body, ignore.case = TRUE)
    m <- regmatches(body, regexec("([A-Za-z_][A-Za-z0-9_]*)\\s*\\(([^()]*)\\)", body))[[1]]
    if (length(m) == 0L) next
    args <- trimws(strsplit(m[[3]], ",")[[1]])
    if (length(args) == 0L || any(!nzchar(args))) next
    sets <- vapply(args, function(a) {
      a <- tolower(a)
      if (!is.na(idx_sets[a])) idx_sets[[a]] else NA_character_
    }, character(1))
    if (any(is.na(sets))) next
    out[[tolower(m[[2]])]] <- unname(sets)
  }
  out
}

#' @keywords internal
#' @noRd
.check_index_domains <- function(extract,
                                 set_extract,
                                 call) {
  if (is.null(extract) || nrow(extract) == 0L || is.null(extract$remainder)) {
    return(invisible(NULL))
  }
  decl <- .declared_arg_sets(extract)
  if (length(decl) == 0L) {
    return(invisible(NULL))
  }
  closure <- .set_superset_closure(set_extract)
  set_case <- set_extract$name
  names(set_case) <- tolower(set_extract$name)

  stmt_rows <- which(tolower(extract$type) %in% c("equation", "formula", "update", "assertion"))
  ref_pattern <- "\\b([A-Za-z_][A-Za-z0-9_]*)\\s*\\(([^()]*)\\)"

  for (n in stmt_rows) {
    text <- .strip_tab_labels(extract$remainder[[n]])
    idx_sets <- .stmt_index_sets(text)
    if (length(idx_sets) == 0L) next
    refs <- regmatches(text, gregexpr(ref_pattern, text, perl = TRUE))[[1]]
    for (ref in refs) {
      nm <- tolower(sub("\\s*\\(.*$", "", ref))
      d <- decl[[nm]]
      if (is.null(d)) next
      args <- trimws(strsplit(sub("^[^(]*\\((.*)\\)$", "\\1", ref), ",")[[1]])
      if (length(args) != length(d)) next
      for (k in seq_along(args)) {
        a <- tolower(args[[k]])
        if (!grepl("^[a-z_][a-z0-9_]*$", a) || is.na(idx_sets[a])) next
        s <- idx_sets[[a]]
        if (tolower(s) == tolower(d[[k]])) next
        if (tolower(s) %in% closure[[tolower(d[[k]])]]) next
        bad_idx <- args[[k]]
        bad_ref <- gsub("\\s+", "", ref)
        stmt_name <- if (tolower(extract$type[[n]]) %=% "equation") {
          regmatches(text, regexpr("[A-Za-z_][A-Za-z0-9_]*", text))
        } else {
          NULL
        }
        bad_stmt <- paste(c(extract$type[[n]], stmt_name), collapse = " ")
        bad_set <- if (!is.na(set_case[tolower(s)])) set_case[[tolower(s)]] else s
        decl_set <- if (!is.na(set_case[tolower(d[[k]])])) set_case[[tolower(d[[k]])]] else d[[k]]
        .cli_action(model_err$index_not_subset,
          action = c("abort", "inform"),
          call = call
        )
      }
    }
  }
  invisible(NULL)
}
