#' @keywords internal
#' @noRd
.chk_tab_names <- function(model,
                           call) {
  typ <- tolower(model$type)
  decl_name <- function(kind) {
    n <- model$name[typ == kind]
    n <- n[!is.na(n) & grepl("^[A-Za-z]", n)]
    return(n)
  }
  coef <- decl_name("coefficient")
  var <- decl_name("variable")
  set <- decl_name("set")
  map <- decl_name("mapping")

  coef_l <- tolower(coef)
  var_l <- tolower(var)
  set_l <- tolower(set)
  map_l <- tolower(map)

  clash <- intersect(coef_l, var_l)
  if (length(clash) > 0L) {
    .cli_action(model_err$name_coef_var,
      action = c("abort", "inform"),
      call = call
    )
  }
  clash <- intersect(coef_l, set_l)
  if (length(clash) > 0L) {
    .cli_action(model_err$name_coef_set,
      action = c("abort", "inform"),
      call = call
    )
  }
  clash <- intersect(var_l, set_l)
  if (length(clash) > 0L) {
    .cli_action(model_err$name_var_set,
      action = c("abort", "inform"),
      call = call
    )
  }

  for (kind in c("coefficient", "variable", "set")) {
    other <- switch(kind,
      coefficient = coef_l,
      variable = var_l,
      set = set_l
    )
    clash <- intersect(map_l, other)
    if (length(clash) > 0L) {
      clash_kind <- kind
      .cli_action(model_err$name_map_clash,
        action = c("abort", "inform"),
        call = call
      )
    }
  }

  for (kind in c("coefficient", "variable", "mapping")) {
    n <- switch(kind,
      coefficient = coef_l,
      variable = var_l,
      mapping = map_l
    )
    if (anyDuplicated(n) > 0L) {
      dup_names <- unique(n[duplicated(n)])
      dup_type <- kind
      .cli_action(model_err$name_dup,
        action = "abort",
        call = call
      )
    }
  }

  res_names <- unique(c(coef_l, var_l, set_l, map_l)[c(coef_l, var_l, set_l, map_l) %in% tab_reserved_words])
  if (length(res_names) > 0L) {
    .cli_action(model_err$name_reserved,
      action = "abort",
      call = call
    )
  }

  bad_names <- coef[grepl("^c_", coef, ignore.case = TRUE)]
  if (length(bad_names) > 0L) {
    .cli_action(model_err$name_c_prefix,
      action = "abort",
      call = call
    )
  }

  # coefficient X + variable p_X/c_X is the supported hand-linearized
  # pair idiom since the solver's section-6 naming resolution (GTAP-AEZ
  # YIELD/p_YIELD); the genuine ambiguity is variable X + variable
  # p_X/c_X coexisting -- the reference token p_X cannot be resolved
  pre <- var_l[grepl("^[pc]_", var_l)]
  base <- substring(pre, 3)
  hit <- base %in% var_l
  if (any(hit)) {
    clash <- paste0(base[hit], "/", pre[hit])
    .cli_action(model_err$name_prefix_clash,
      action = c("abort", "inform"),
      call = call
    )
  }

  max_len <- 255L
  all_names <- c(coef, var, set, map)
  long_names <- unique(all_names[nchar(all_names) > max_len])
  if (length(long_names) > 0L) {
    long_names <- paste0(substr(long_names, 1, 20), "...")
    .cli_action(model_err$name_too_long,
      action = "abort",
      call = call
    )
  }

  # C0/C1a levels: p_-leading levels names are carried by the solver's
  # gen_lv pair rename; c_-leading names stay fatal -- the solver
  # preprocess folds their value references into p_ column references
  # on equation/update lines before the rename can see them (solver
  # fatal mirrored here)
  lev <- typ == "variable" & !is.na(model$qualifier_list) &
    grepl("\\blevels\\b", model$qualifier_list, ignore.case = TRUE)
  bad_lev <- lev & grepl("^c_", tolower(model$name))
  if (any(bad_lev)) {
    bad_names <- unique(model$name[bad_lev])
    .cli_action(model_err$levels_prefix_name,
      action = c("abort", "inform"),
      call = call
    )
  }
  return(invisible(NULL))
}
