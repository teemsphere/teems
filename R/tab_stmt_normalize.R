#' @keywords internal
#' @noRd
.protect_label_semicolons <- function(text) {
  labels <- gregexpr("#[^#]*#", text, perl = TRUE)
  regmatches(text, labels) <- lapply(
    regmatches(text, labels),
    \(l) gsub(";", ",", l, fixed = TRUE)
  )
  return(text)
}

#' @keywords internal
#' @noRd
.normalize_quantifier_spacing <- function(statements) {
  statements <- gsub(
    "\\(\\s*all\\s*,\\s*([A-Za-z0-9_@]+)\\s*,\\s*",
    "(all,\\1,",
    statements,
    ignore.case = TRUE,
    perl = TRUE
  )
  return(statements)
}

#' @keywords internal
#' @noRd
.normalize_brace_brackets <- function(statement) {
  if (!grepl("{", statement, fixed = TRUE)) {
    return(statement)
  }
  chars <- strsplit(statement, "", fixed = TRUE)[[1]]
  stack <- character(0)
  in_label <- FALSE
  for (i in seq_along(chars)) {
    ch <- chars[i]
    if (ch == "#") {
      in_label <- !in_label
      next
    }
    if (in_label) {
      next
    }
    if (ch == "{") {
      j <- i - 1L
      while (j >= 1L && chars[j] == " ") {
        j <- j - 1L
      }
      k <- j
      while (k >= 1L && grepl("[A-Za-z0-9_@]", chars[k])) {
        k <- k - 1L
      }
      word <- tolower(paste(chars[seq_len(j)][seq.int(k + 1L, length.out = j - k)], collapse = ""))
      if (word %in% c("sum", "prod", "maxs", "mins")) {
        stack <- c(stack, "keep")
      } else {
        stack <- c(stack, "conv")
        chars[i] <- "["
      }
    } else if (ch == "}") {
      if (length(stack) > 0L) {
        if (stack[length(stack)] == "conv") {
          chars[i] <- "]"
        }
        stack <- stack[-length(stack)]
      }
    }
  }
  normalized <- paste(chars, collapse = "")
  return(normalized)
}

#' @keywords internal
#' @noRd
.normalize_qualifier_order <- function(statement) {
  kw <- tolower(sub("^\\s*([A-Za-z_]+).*$", "\\1", statement))
  if (!kw %in% c("variable", "coefficient")) {
    return(statement)
  }
  head <- sub("^(\\s*[A-Za-z_]+\\s*).*$", "\\1", statement)
  rest <- substr(statement, nchar(head) + 1L, nchar(statement))
  groups <- character(0)
  kinds <- character(0)
  pos <- 1L
  n <- nchar(rest)
  while (pos <= n && substr(rest, pos, pos) == "(") {
    depth <- 0L
    end <- NA_integer_
    for (i in seq.int(pos, n)) {
      ch <- substr(rest, i, i)
      if (ch == "(") {
        depth <- depth + 1L
      } else if (ch == ")") {
        depth <- depth - 1L
        if (depth == 0L) {
          end <- i
          break
        }
      }
    }
    if (is.na(end)) {
      return(statement)
    }
    group <- substr(rest, pos, end)
    inner <- tolower(trimws(substr(group, 2L, nchar(group) - 1L)))
    first <- sub("^([a-z_]+).*$", "\\1", inner)
    kind <- if (grepl("^all\\b", inner)) {
      "quantifier"
    } else if (first %in% c(
      "change", "percent_change", "linear", "levels", "parameter",
      "non_parameter", "real", "integer", "ge", "gt", "le", "lt",
      "default", "orig_level", "vpqtype"
    )) {
      "qualifier"
    } else {
      NA_character_
    }
    if (is.na(kind)) {
      break
    }
    groups <- c(groups, group)
    kinds <- c(kinds, kind)
    pos <- end + 1L
    while (pos <= n && substr(rest, pos, pos) == " ") {
      pos <- pos + 1L
    }
  }
  if (length(kinds) < 2L) {
    return(statement)
  }
  first_quant <- match("quantifier", kinds)
  if (is.na(first_quant) || !any(kinds[seq.int(first_quant, length(kinds))] == "qualifier")) {
    return(statement)
  }
  ordered <- c(groups[kinds == "qualifier"], groups[kinds == "quantifier"])
  tail <- substr(rest, pos, n)
  normalized <- paste0(head, paste(ordered, collapse = " "), " ", tail)
  return(normalized)
}

#' @importFrom purrr map_chr
#' @keywords internal
#' @noRd
.flatten_ranked_sets <- function(statements,
                                 call) {
  pattern <- "^(\\s*set\\s+[A-Za-z0-9_@]+\\s*(?:#[^#]*#)?\\s*=\\s*)([A-Za-z0-9_@]+)\\s+ranked\\s+(?:up|down)\\s+by\\s+([A-Za-z0-9_@]+)\\s*$"
  hits <- grepl(pattern, statements, ignore.case = TRUE, perl = TRUE)
  if (any(hits)) {
    ranked_set <- purrr::map_chr(statements[hits], \(s) {
      sub("^\\s*set\\s+([A-Za-z0-9_@]+).*$", "\\1", s, ignore.case = TRUE)
    })
    rank_var <- sub(pattern, "\\3", statements[hits], ignore.case = TRUE, perl = TRUE)
    statements[hits] <- sub(pattern, "\\1\\2", statements[hits], ignore.case = TRUE, perl = TRUE)
    .cli_action(model_info$ranked_set_flattened,
      action = c("inform", "inform"),
      call = call
    )
  }
  return(statements)
}

#' @importFrom purrr map_chr
#' @keywords internal
#' @noRd
.normalize_statements <- function(statements,
                                  call) {
  statements <- .normalize_quantifier_spacing(statements)
  statements <- purrr::map_chr(statements, .normalize_brace_brackets)
  statements <- purrr::map_chr(statements, .normalize_qualifier_order)
  statements <- .flatten_ranked_sets(statements, call = call)
  return(statements)
}

#' @keywords internal
#' @noRd
.drop_set_writes <- function(statements) {
  statements <- statements[!grepl(
    "^\\s*write\\s*\\(\\s*(set|by_elements)\\s*\\)",
    statements,
    ignore.case = TRUE
  )]
  return(statements)
}

#' @keywords internal
#' @noRd
.continue_assertion_labels <- function(statements) {
  for (i in seq_along(statements)) {
    if (i == 1L || !startsWith(statements[i], "#")) {
      next
    }
    prev_kw <- tolower(sub("^\\s*([A-Za-z_]+).*$", "\\1", statements[i - 1L]))
    if (prev_kw == "assertion") {
      statements[i] <- paste("Assertion", statements[i])
    }
  }
  return(statements)
}
