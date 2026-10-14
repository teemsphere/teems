# The examples of an Rd topic as parsed expressions, \dontrun and
# \donttest blocks included. `subs` maps placeholder strings in the code
# to real values; every placeholder must be present and no "path/to/"
# may survive, so an example edited away from what the test supplies
# fails here rather than silently testing something else.
example_exprs <- function(topic, subs = character(0)) {
  man <- file.path("..", "..", "man", paste0(topic, ".Rd"))
  rd <- if (file.exists(man)) {
    tools::parse_Rd(man)
  } else {
    tools::Rd_db("teems")[[paste0(topic, ".Rd")]]
  }
  ex <- tempfile(fileext = ".R")
  tools::Rd2ex(rd, ex, commentDontrun = FALSE, commentDonttest = FALSE)
  code <- readLines(ex)
  for (from in names(subs)) {
    if (!any(grepl(from, code, fixed = TRUE))) {
      stop("Placeholder not in the ", topic, " examples: ", from)
    }
    code <- gsub(from, subs[[from]], code, fixed = TRUE)
  }
  if (any(grepl("path/to/", code, fixed = TRUE))) {
    stop("Unsubstituted placeholder path in the ", topic, " examples")
  }
  exprs <- as.list(parse(text = code, keep.source = FALSE))
  return(exprs)
}

# Evaluate example expressions in `env` the way R CMD check does (visible
# values printed, messages muffled) and return each value, named by the
# function the expression calls (directly or on the right of an
# assignment).
run_examples <- function(exprs, env) {
  call_name <- function(e) {
    if (is.call(e) && identical(e[[1]], as.name("<-"))) {
      e <- e[[3]]
    }
    if (is.call(e) && is.name(e[[1]])) as.character(e[[1]]) else ""
  }
  values <- lapply(exprs, function(e) {
    suppressMessages({
      v <- withVisible(eval(e, env))
      if (v$visible) {
        utils::capture.output(print(v$value))
      }
    })
    v$value
  })
  names(values) <- vapply(exprs, call_name, character(1))
  return(values)
}

example_env <- function(...) {
  env <- new.env(parent = globalenv())
  list2env(list(...), env)
  return(env)
}
