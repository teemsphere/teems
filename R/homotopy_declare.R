#' @keywords internal
#' @noRd
.declare_homotopy <- function(statements) {
  is_eq <- grepl("^Equation\\b", statements)
  if (!any(is_eq)) {
    return(statements)
  }
  homo <- rep(NA_character_, length(statements))
  for (i in which(is_eq)) {
    toks <- .tab_qualifier_groups(statements[i])$groups
    toks <- tolower(gsub("[[:space:]]", "", unlist(strsplit(toks, ",", fixed = TRUE))))
    if (!"levels" %in% toks) {
      next
    }
    hit <- toks[grepl("^add_homotopy(=.+)?$", toks)]
    if (length(hit) > 0L) {
      homo[i] <- if (grepl("=", hit[1], fixed = TRUE)) sub("^add_homotopy=", "", hit[1]) else "HOMOTOPY"
    }
  }
  if (all(is.na(homo))) {
    return(statements)
  }
  is_var <- grepl("^Variable\\b", statements)
  declared <- tolower(vapply(statements[is_var], .homotopy_decl_name, character(1)))
  for (h in unique(homo[!is.na(homo)])) {
    if (tolower(h) %in% declared) {
      next
    }
    first <- which(tolower(homo) == tolower(h))[1]
    decl <- c(
      paste0("Variable (levels,change) ", h, " # homotopy variable (ADD_HOMOTOPY, GEMPACK manual 26.7) #"),
      paste0("Formula (initial) ", h, " = -1")
    )
    statements <- append(statements, decl, after = first - 1L)
    homo <- append(homo, c(NA_character_, NA_character_), after = first - 1L)
  }
  return(statements)
}

#' @keywords internal
#' @noRd
.homotopy_decl_name <- function(statement) {
  rest <- sub("^\\s*[A-Za-z_]+", "", gsub("#[^#]*#", "", statement))
  repeat {
    if (!grepl("^\\s*\\(", rest)) {
      break
    }
    rest <- sub("^\\s*\\([^)]*\\)", "", rest)
  }
  name <- sub("^\\s*([A-Za-z0-9_@]+).*$", "\\1", rest)
  return(name)
}
