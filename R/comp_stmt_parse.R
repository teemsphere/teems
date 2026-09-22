#' @keywords internal
#' @noRd
.parse_comp_stmt <- function(statement,
                             call) {
  txt <- gsub("#[^#]*#", " ", statement)
  m <- regmatches(
    txt,
    regexec("^\\s*complementarity\\s*\\(([^)]*)\\)\\s*(.*)$",
      txt,
      ignore.case = TRUE
    )
  )[[1]]
  if (length(m) == 0L) {
    bad_stmt <- trimws(statement)
    .cli_action(model_err$comp_malformed,
      action = c("abort", "inform"),
      call = call
    )
  }
  entries <- strsplit(m[2], ",")[[1]]
  keys <- character(0)
  vals <- character(0)
  for (e in entries) {
    kv <- strsplit(e, "=")[[1]]
    if (length(kv) != 2L) {
      bad_stmt <- trimws(statement)
      .cli_action(model_err$comp_malformed,
        action = c("abort", "inform"),
        call = call
      )
    }
    keys <- c(keys, tolower(trimws(kv[1])))
    vals <- c(vals, trimws(kv[2]))
  }
  if (any(!keys %in% c("variable", "lower_bound", "upper_bound")) ||
    anyDuplicated(keys) > 0L) {
    bad_stmt <- trimws(statement)
    .cli_action(model_err$comp_malformed,
      action = c("abort", "inform"),
      call = call
    )
  }
  if (!"variable" %in% keys) {
    bad_stmt <- trimws(statement)
    .cli_action(model_err$comp_missing_variable,
      action = "abort",
      call = call
    )
  }
  rem <- trimws(m[3])
  name <- regmatches(rem, regexec("^([A-Za-z0-9_]+)", rem))[[1]]
  if (length(name) == 0L) {
    bad_stmt <- trimws(statement)
    .cli_action(model_err$comp_malformed,
      action = c("abort", "inform"),
      call = call
    )
  }
  name <- name[2]
  rem <- trimws(substring(rem, nchar(name) + 1L))
  n_quant <- length(gregexpr("\\(\\s*all\\s*,", rem, ignore.case = TRUE)[[1]])
  if (gregexpr("\\(\\s*all\\s*,", rem, ignore.case = TRUE)[[1]][1] == -1L) {
    n_quant <- 0L
  }
  stmt <- list(
    name = name,
    comp_var = vals[keys == "variable"],
    lower_bound = if ("lower_bound" %in% keys) {
      vals[keys == "lower_bound"]
    } else {
      NULL
    },
    upper_bound = if ("upper_bound" %in% keys) {
      vals[keys == "upper_bound"]
    } else {
      NULL
    },
    n_quant = n_quant
  )
  return(stmt)
}
