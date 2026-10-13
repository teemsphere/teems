#' @importFrom data.table copy fsetdiff fsetequal set .N
#' @importFrom utils capture.output head
#' @keywords internal
#' @noRd
.check_scen_coverage <- function(template,
                                 shock,
                                 sets,
                                 call) {
  .present <- NULL
  .N <- NULL
  var_name <- shock$var
  input_tup <- shock$input[, !"Value"]
  extra_tuples <- data.table::fsetdiff(input_tup, template)
  if (nrow(extra_tuples) > 0L) {
    n_extra <- nrow(extra_tuples)
    extra_tuples <- utils::head(do.call(paste, c(extra_tuples, sep = ",")), 10L)
    .cli_action(shk_err$scen_extra_tup,
      action = c("abort", "inform"),
      call = call
    )
  }
  if (data.table::fsetequal(template, input_tup)) {
    is_full <- TRUE
    return(is_full)
  }
  missing_all <- data.table::fsetdiff(template, input_tup)
  if (!isTRUE(shock$partial)) {
    n_missing_tuples <- nrow(missing_all)
    missing_tuples <- utils::capture.output(print(missing_all))[-c(1:2)]
    .cli_action(
      shk_err$scen_missing_tup,
      action = c("abort", "inform", "inform", "inform"),
      call = call
    )
  }
  key_col <- setdiff(shock$set, "Year")
  agg_col <- paste0(".agg_", key_col)
  grp <- data.table::copy(template)
  for (i in seq_along(key_col)) {
    upper <- shock$ls_upper[match(key_col[i], shock$set)]
    map_tbl <- sets$mapping[[upper]]
    r_idx <- match(grp[[key_col[i]]], tolower(map_tbl[, 1][[1]]))
    data.table::set(grp, j = agg_col[i], value = map_tbl[, 2][[1]][r_idx])
  }
  in_template <- grp[input_tup, which = TRUE, on = shock$set, nomatch = 0L]
  data.table::set(grp, j = ".present", value = seq_len(nrow(grp)) %in% in_template)
  cover <- grp[, list(n = .N, n_in = sum(.present)), by = agg_col]
  part <- cover[cover$n_in > 0L & cover$n_in < cover$n, ]
  if (nrow(part) > 0L) {
    n_part <- nrow(part)
    part_tuples <- utils::head(do.call(paste, c(part[, agg_col, with = FALSE], sep = ",")), 10L)
    missing_grp <- grp[grp[[".present"]] == FALSE, ][part, on = agg_col, nomatch = 0L]
    n_missing_tuples <- nrow(missing_grp)
    missing_tuples <- utils::head(do.call(paste, c(missing_grp[, shock$set, with = FALSE], sep = ",")), 10L)
    .cli_action(shk_err$scen_partial_tup,
      action = c("abort", "inform", "inform"),
      call = call
    )
  }
  if (.o_verbose()) {
    covered <- cover[cover$n_in > 0L, ]
    n_cov <- nrow(covered)
    n_agg <- nrow(cover)
    cov_tuples <- utils::head(do.call(paste, c(covered[, agg_col, with = FALSE], sep = ",")), 10L)
    .cli_action(deploy_info$scen_partial,
      action = c("inform", "inform"),
      call = call
    )
  }
  is_full <- FALSE
  return(is_full)
}
