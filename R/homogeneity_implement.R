#' @importFrom data.table data.table
#' @importFrom tibble tibble
#' @keywords internal
#' @noRd
.implement_homogeneity <- function(cmf_path,
                                   type,
                                   simulate,
                                   solve_args,
                                   call) {
  run_dir <- dirname(cmf_path)
  metadata_path <- file.path(run_dir, "metadata.rds")
  vpq <- if (file.exists(metadata_path)) readRDS(metadata_path)$vpqtype else NULL
  if (is.null(vpq)) {
    .cli_action(solve_err$homog_no_types,
      action = "abort",
      call = call
    )
  }
  if (all(vpq == "unspecified")) {
    .cli_action(solve_err$homog_untyped,
      action = "abort",
      call = call
    )
  }
  work_cmf <- .homogeneity_workspace(cmf_path)
  sol_prefix <- file.path(dirname(work_cmf), "out", "variables", "bin", "sol.")
  .implement_solve(
    args_list = .homogeneity_solve_args(work_cmf, list(suppress_outputs = TRUE)),
    call = call,
    solmed = "probe",
    extra_flags = "-jacdump 2"
  )
  jac <- .parse_jacobian(sol_prefix)
  if (is.null(jac)) {
    .cli_action(solve_err$homog_no_jacobian,
      action = "abort",
      call = call
    )
  }
  meta <- .parse_solution_meta(sol_prefix = sol_prefix)
  var_extract <- .retrieve_tab_comp(
    tab_path = .get_output_paths(cmf_path = work_cmf)[["tab"]],
    type = "variable",
    call = call
  )$variable
  pre <- .var_presim_levels(sol_prefix, meta$var_union, var_extract)
  z <- .homogeneity_z(meta$var_union, vpq, pre, type)
  rows <- .homogeneity_rows(jac, z, meta)
  output <- list(
    type = type,
    typed = sum(vpq != "unspecified"),
    equations = .homogeneity_blocks(rows),
    elements = rows
  )
  if (simulate) {
    output$variables <- .homogeneity_simulate(
      work_cmf = work_cmf,
      vpq = vpq,
      pre = pre,
      var_tbl = meta$var_union,
      type = type,
      solve_args = solve_args,
      call = call
    )
  }
  return(output)
}

#' @keywords internal
#' @noRd
.homogeneity_workspace <- function(cmf_path) {
  run_dir <- dirname(cmf_path)
  work <- paste0(run_dir, "_homogeneity")
  unlink(work, recursive = TRUE)
  dir.create(work)
  files <- list.files(run_dir, full.names = TRUE)
  files <- files[!dir.exists(files) & !startsWith(basename(files), "_temp_")]
  file.copy(files, work)
  .out_mkdir(write_dir = work)
  return(file.path(work, basename(cmf_path)))
}

#' @keywords internal
#' @noRd
.homogeneity_solve_args <- function(cmf_path,
                                    overrides) {
  args_list <- list(
    cmf_path = cmf_path,
    solution_method = "Johansen",
    matrix_method = "LU",
    n_subintervals = 1L,
    steps = NULL,
    n_tasks = 1L,
    n_threads = 1L,
    precision = "single",
    verbosity = 1L,
    suppress_outputs = FALSE,
    terminal_run = FALSE,
    complementarity = NULL,
    adaptive = "no",
    eps_tolerance = 0.01,
    max_retries = 3L,
    retry_adjust = 0.5
  )
  args_list <- c(args_list, .solver_extra_args())
  for (nm in intersect(names(overrides), names(args_list))) {
    args_list[nm] <- list(overrides[[nm]])
  }
  return(args_list)
}

#' @keywords internal
#' @noRd
.homogeneity_expected <- function(vpq_type, type) {
  moves <- if (type == "nominal") c("value", "price") else c("value", "quantity")
  out <- ifelse(vpq_type %in% moves, 1, 0)
  out[is.na(vpq_type) | vpq_type == "unspecified"] <- NA_real_
  return(out)
}

#' @keywords internal
#' @noRd
.homogeneity_z <- function(var_tbl,
                           vpq,
                           pre,
                           type) {
  z <- rep(NA_real_, sum(var_tbl$matsize))
  start <- c(0, cumsum(var_tbl$matsize))
  for (i in seq_len(nrow(var_tbl))) {
    nm <- var_tbl$cofname[i]
    e <- .homogeneity_expected(unname(vpq[nm]), type)
    if (is.na(e)) {
      next
    }
    idx <- start[i] + seq_len(var_tbl$matsize[i])
    if (isTRUE(var_tbl$change_real[i])) {
      if (is.null(pre[[nm]])) {
        next
      }
      z[idx] <- e * as.numeric(pre[[nm]]) / 100
    } else {
      z[idx] <- e
    }
  }
  return(z)
}

#' @keywords internal
#' @noRd
.homogeneity_labels <- function(eq, meta) {
  sets <- meta$set_union
  ele <- meta$setele$mapped_ele
  n <- eq$nrows
  label <- rep(eq$name, n)
  set_names <- unlist(eq$sets)
  if (length(set_names) == 0L || n == 0L) {
    return(label)
  }
  k <- seq_len(n) - 1L
  idx <- character(n)
  parts <- lapply(set_names, \(s) {
    j <- match(tolower(s), tolower(sets$setname))
    pos <- k %% sets$size[j]
    k <<- k %/% sets$size[j]
    ele[sets$begadd[j] + pos + 1L]
  })
  paste0(eq$name, "(", do.call(paste, c(parts, sep = ",")), ")")
}

#' @importFrom data.table data.table
#' @keywords internal
#' @noRd
.homogeneity_rows <- function(jac, z, meta) {
  term <- jac$value * z[jac$col + 1L]
  dt <- data.table::data.table(row = jac$row, term = term)
  agg <- dt[, list(
    tested = !anyNA(term),
    rel_sum = abs(sum(term)),
    sum_abs = sum(abs(term))
  ), by = "row"]
  rows <- data.table::data.table(row = seq_len(jac$nrow) - 1L)
  rows <- merge(rows, agg, by = "row", all.x = TRUE, sort = TRUE)
  rows$tested[is.na(rows$tested)] <- TRUE
  rows$rel_sum[is.na(rows$rel_sum)] <- 0
  rows$sum_abs[is.na(rows$sum_abs)] <- 0
  rows$rel_ratio <- ifelse(rows$sum_abs > 0, rows$rel_sum / rows$sum_abs, 0)
  rows$err <- ifelse(rows$tested, pmin(rows$rel_sum, rows$rel_ratio), NA_real_)
  eqs <- jac$equations
  rows$equation <- rep(eqs$name, eqs$nrows)
  rows$element <- unlist(lapply(seq_len(nrow(eqs)), \(i) {
    .homogeneity_labels(list(name = eqs$name[i], nrows = eqs$nrows[i], sets = eqs$sets[[i]]), meta)
  }))
  rows <- rows[, c("equation", "element", "tested", "err", "rel_sum", "rel_ratio", "sum_abs")]
  return(rows)
}

#' @importFrom tibble tibble
#' @keywords internal
#' @noRd
.homogeneity_blocks <- function(rows) {
  blocks <- split(rows, factor(rows$equation, levels = unique(rows$equation)))
  out <- tibble::tibble(
    equation = names(blocks),
    rows = vapply(blocks, nrow, integer(1)),
    tested = vapply(blocks, \(b) all(b$tested), logical(1)),
    max_err = vapply(blocks, \(b) if (any(b$tested)) max(b$err, na.rm = TRUE) else NA_real_, numeric(1)),
    worst = vapply(blocks, \(b) if (any(b$tested)) b$element[which.max(b$err)] else NA_character_, character(1))
  )
  out <- out[order(!out$tested, -ifelse(is.na(out$max_err), -Inf, out$max_err)), ]
  return(out)
}

#' @importFrom tibble tibble
#' @keywords internal
#' @noRd
.homogeneity_simulate <- function(work_cmf,
                                  vpq,
                                  pre,
                                  var_tbl,
                                  type,
                                  solve_args,
                                  call) {
  work <- dirname(work_cmf)
  cmf <- readLines(work_cmf)
  shock_line <- grep("^shock ", cmf, value = TRUE)
  cls_line <- grep("^closure ", cmf, value = TRUE)
  shf <- file.path(work, basename(sub('^shock "([^"]*)";.*$', "\\1", shock_line)))
  cls <- file.path(work, basename(sub('^closure "([^"]*)";.*$', "\\1", cls_line)))
  entries <- trimws(gsub(";", "", readLines(cls)))
  entries <- entries[nzchar(entries) & !tolower(entries) %in% c("exogenous", "rest endogenous")]
  change <- var_tbl$change_real
  names(change) <- var_tbl$cofname
  unshocked <- character(0)
  shocks <- character(0)
  for (entry in entries) {
    nm <- tolower(sub("\\(.*$", "", entry))
    e <- .homogeneity_expected(unname(vpq[nm]), type)
    if (is.na(e) || e == 0) {
      next
    }
    size <- 1
    if (isTRUE(change[[nm]])) {
      level <- unique(as.numeric(pre[[nm]]))
      if (length(level) != 1L) {
        unshocked <- c(unshocked, nm)
        next
      }
      size <- level / 100
    }
    shocks <- c(shocks, sprintf("Shock %s = uniform %s;", entry, format(size, digits = 15)))
  }
  writeLines(if (length(shocks) > 0L) shocks else "Shock ;", shf)
  solve_args$solution_method <- "Johansen"
  out <- .implement_solve(
    args_list = .homogeneity_solve_args(work_cmf, solve_args),
    call = call
  )
  out <- out[out$type == "variable", ]
  res <- lapply(seq_len(nrow(out)), \(i) {
    nm <- tolower(out$name[i])
    e <- .homogeneity_expected(unname(vpq[nm]), type)
    d <- out$dat[[i]]
    if (is.na(e) || nm %in% unshocked) {
      return(NULL)
    }
    if (isTRUE(change[[nm]])) {
      if (!"PreLevel" %in% names(d)) {
        return(NULL)
      }
      expected <- e * d$PreLevel / 100
    } else {
      expected <- rep(e, nrow(d))
    }
    err <- abs(d$Value - expected) / pmax(1, pmin(abs(d$Value), abs(expected)))
    keys <- setdiff(names(d), c("Value", "error_estimate", "PreLevel", "PostLevel", "Change", "PercentChange", "Year"))
    worst <- which.max(err)
    label <- if (length(keys) > 0L) {
      paste0(out$name[i], "(", paste(unlist(d[worst, keys, with = FALSE]), collapse = ","), ")")
    } else {
      out$name[i]
    }
    tibble::tibble(
      variable = out$name[i],
      vpqtype = unname(vpq[nm]),
      expected = expected[worst],
      max_err = max(err),
      worst = label
    )
  })
  variables <- do.call(rbind, res)
  variables <- variables[order(-variables$max_err), ]
  attr(variables, "unshocked") <- unshocked
  return(variables)
}
