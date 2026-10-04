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
  .implement_solve(
    args_list = .homogeneity_solve_args(work_cmf, list(suppress_outputs = TRUE)),
    call = call,
    solmed = "probe",
    extra_flags = paste("-jacdump 2 -zdivshift", .homogeneity_zdiv_shift)
  )
  shifted <- .parse_jacobian(sol_prefix)
  if (is.null(shifted)) {
    .cli_action(solve_err$homog_no_jacobian,
      action = "abort",
      call = call
    )
  }
  pinned <- attr(rows, "pinned")
  rows$zero_flow <- rows$zero_flow | .homogeneity_moved_rows(jac, shifted)
  attr(rows, "pinned") <- NULL
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
      pinned = pinned,
      type = type,
      solve_args = solve_args,
      call = call
    )
  }
  return(output)
}

.homogeneity_zdiv_shift <- 1e-3

#' @importFrom data.table data.table
#' @keywords internal
#' @noRd
.homogeneity_moved_rows <- function(a, b) {
  da <- data.table::data.table(row = a$row, col = a$col, va = a$value)
  db <- data.table::data.table(row = b$row, col = b$col, vb = b$value)
  both <- merge(da, db, by = c("row", "col"), all = TRUE)
  both$va[is.na(both$va)] <- 0
  both$vb[is.na(both$vb)] <- 0
  moved_rows <- unique(both$row[abs(both$va - both$vb) > 1e-9 * (1 + abs(both$va))])
  return((seq_len(a$nrow) - 1L) %in% moved_rows)
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
  work_cmf <- file.path(work, basename(cmf_path))
  return(work_cmf)
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
.homogeneity_col_labels <- function(cols, meta) {
  vars <- meta$var_union
  sets <- meta$set_union
  ele <- meta$setele$mapped_ele
  start <- c(0, cumsum(vars$matsize))
  i <- findInterval(cols, start, rightmost.closed = FALSE)
  vapply(seq_along(cols), \(k) {
    v <- i[k]
    off <- cols[k] - start[v]
    size <- vars$size[v]
    if (size == 0L) {
      pick <- tolower(vars$cofname[v])
      return(pick)
    }
    ids <- as.integer(strsplit(vars$setid[v], ",", fixed = TRUE)[[1]][seq_len(size)])
    dims <- sets$size[ids + 1L]
    stride <- rev(cumprod(rev(c(dims[-1], 1))))
    idx <- off %/% stride %% dims
    tolower(paste0(vars$cofname[v], "(", paste(ele[sets$begadd[ids + 1L] + idx + 1L], collapse = ","), ")"))
  }, character(1))
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
  zc <- z[jac$col + 1L]
  dt <- data.table::data.table(row = jac$row, term = term, nz = jac$value != 0,
                               moving = jac$value != 0 & !is.na(zc) & zc != 0)
  agg <- dt[, list(
    tested = !anyNA(term),
    rel_sum = abs(sum(term)),
    sum_abs = sum(abs(term)),
    pinned = sum(nz) == 1L,
    single = sum(moving) == 1L
  ), by = "row"]
  rows <- data.table::data.table(row = seq_len(jac$nrow) - 1L)
  rows <- merge(rows, agg, by = "row", all.x = TRUE, sort = TRUE)
  rows$tested[is.na(rows$tested)] <- TRUE
  rows$rel_sum[is.na(rows$rel_sum)] <- 0
  rows$sum_abs[is.na(rows$sum_abs)] <- 0
  rows$pinned[is.na(rows$pinned)] <- FALSE
  rows$single[is.na(rows$single)] <- FALSE
  rows$rel_ratio <- ifelse(rows$sum_abs > 0, rows$rel_sum / rows$sum_abs, 0)
  rows$err <- ifelse(rows$tested, pmin(rows$rel_sum, rows$rel_ratio), NA_real_)
  eqs <- jac$equations
  rows$equation <- rep(eqs$name, eqs$nrows)
  rows$element <- unlist(lapply(seq_len(nrow(eqs)), \(i) {
    .homogeneity_labels(list(name = eqs$name[i], nrows = eqs$nrows[i], sets = eqs$sets[[i]]), meta)
  }))
  pinned_rows <- rows$row[rows$pinned]
  pinned_cols <- jac$col[jac$row %in% pinned_rows & jac$value != 0]
  uses_pinned <- unique(jac$row[jac$col %in% pinned_cols & jac$value != 0])
  rows$zero_flow <- rows$pinned | rows$single | rows$row %in% uses_pinned
  rows <- rows[, c("equation", "element", "tested", "err", "rel_sum", "rel_ratio", "sum_abs", "zero_flow")]
  attr(rows, "pinned") <- .homogeneity_col_labels(pinned_cols, meta)
  return(rows)
}

#' @importFrom tibble tibble
#' @keywords internal
#' @noRd
.homogeneity_blocks <- function(rows) {
  blocks <- split(rows, factor(rows$equation, levels = unique(rows$equation)))
  counted <- \(b) b$tested & !b$zero_flow
  out <- tibble::tibble(
    equation = names(blocks),
    rows = vapply(blocks, nrow, integer(1)),
    tested = vapply(blocks, \(b) all(b$tested), logical(1)),
    zero_flow = vapply(blocks, \(b) sum(b$zero_flow), integer(1)),
    max_err = vapply(blocks, \(b) if (any(counted(b))) max(b$err[counted(b)]) else NA_real_, numeric(1)),
    worst = vapply(blocks, \(b) if (any(counted(b))) b$element[counted(b)][which.max(b$err[counted(b)])] else NA_character_, character(1))
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
                                  pinned,
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
  shifted <- .implement_solve(
    args_list = .homogeneity_solve_args(work_cmf, solve_args),
    call = call,
    extra_flags = paste("-zdivshift", .homogeneity_zdiv_shift)
  )
  shifted <- shifted[shifted$type == "variable", ]
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
    labels <- if (length(keys) > 0L) {
      paste0(nm, "(", do.call(paste, c(as.list(d[, keys, with = FALSE]), sep = ",")), ")")
    } else {
      rep(nm, nrow(d))
    }
    moved <- abs(d$Value - shifted$dat[[match(out$name[i], shifted$name)]]$Value) > 1e-6 * (1 + abs(d$Value)) |
      tolower(labels) %in% pinned |
      (d$Value == 0 & expected != 0)
    err[moved] <- 0
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
      zero_flow = sum(moved),
      max_err = max(err),
      worst = label
    )
  })
  variables <- do.call(rbind, res)
  variables <- variables[order(-variables$max_err), ]
  attr(variables, "unshocked") <- unshocked
  return(variables)
}
