#' @keywords internal
#' @noRd
.chk_tab_postsim <- function(model,
                             call) {
  if (!any(model$postsim)) {
    return(invisible(NULL))
  }
  ps <- model$postsim
  typ <- tolower(model$type)
  ps_names <- tolower(model$name[ps & typ %in% c("coefficient", "set", "subset", "file", "mapping")])
  ps_names <- unique(ps_names[!is.na(ps_names) & nzchar(ps_names)])
  coef_ord <- tolower(model$name[!ps & typ == "coefficient"])
  coef_ps <- tolower(model$name[ps & typ == "coefficient"])
  var_names <- tolower(model$name[typ == "variable"])

  if (length(ps_names) > 0L) {
    exec_ord <- which(!ps & typ %in% c("formula", "equation", "update", "assertion", "read"))
    scan <- tolower(gsub('"[^"]*"', "", gsub("#[^#]*#", "", model$tab[exec_ord])))
    hits <- vapply(
      ps_names,
      \(n) any(grepl(paste0("\\b", n, "\\b"), scan)),
      logical(1)
    )
    if (any(hits)) {
      bad_refs <- ps_names[hits]
      .cli_action(model_err$postsim_scope,
        action = c("abort", "inform"),
        call = call
      )
    }
  }

  rd <- typ == "read"
  files_ord <- unique(tolower(model$file[rd & !ps]))
  files_ps <- unique(tolower(model$file[rd & ps]))
  bad_files <- setdiff(intersect(files_ord, files_ps), NA)
  if (length(bad_files) > 0L) {
    .cli_action(model_err$postsim_same_file,
      action = c("abort", "inform"),
      call = call
    )
  }

  tgt <- tolower(model$name[rd & ps])
  tgt <- tgt[!is.na(tgt) & nzchar(tgt)]
  bad_targets <- unique(tgt[tgt %in% var_names])
  if (length(bad_targets) > 0L) {
    .cli_action(model_err$postsim_read_var,
      action = "abort",
      call = call
    )
  }
  bad_targets <- unique(tgt[tgt %in% coef_ord & !tgt %in% coef_ps])
  if (length(bad_targets) > 0L) {
    .cli_action(model_err$postsim_read_ord,
      action = "abort",
      call = call
    )
  }
  bad_targets <- unique(tgt[!tgt %in% c(coef_ord, coef_ps)])
  if (length(bad_targets) > 0L) {
    .cli_action(model_err$postsim_read_undecl,
      action = "abort",
      call = call
    )
  }

  psf <- which(ps & typ == "formula" & !is.na(model$comp1))
  if (length(psf) > 0L) {
    lhs <- .formula_lhs_name(model$comp1[psf])
    bad_lhs <- unique(lhs[lhs %in% var_names])
    if (length(bad_lhs) > 0L) {
      .cli_action(model_err$postsim_lhs_var,
        action = "abort",
        call = call
      )
    }
    bad_lhs <- unique(lhs[lhs %in% coef_ord & !lhs %in% coef_ps])
    if (length(bad_lhs) > 0L) {
      .cli_action(model_err$postsim_lhs_ord,
        action = "abort",
        call = call
      )
    }
  }
  return(invisible(NULL))
}
