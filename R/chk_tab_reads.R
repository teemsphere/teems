#' @keywords internal
#' @noRd
.chk_tab_reads <- function(model,
                           call) {
  typ <- tolower(model$type)
  reads <- which(typ == "read")
  map_names <- tolower(model$name[typ == "mapping"])
  fml_lhs <- .formula_lhs_name(model$comp1[typ == "formula" & !is.na(model$comp1)])
  projected <- tolower(model$name[typ == "mapping" &
    grepl("project", model$qualifier_list, ignore.case = TRUE)])
  map_names <- setdiff(map_names, c(fml_lhs, projected))
  if (length(reads) == 0L) {
    if (length(map_names) > 0L) {
      bad_maps <- unique(map_names)
      .cli_action(model_err$map_read_missing,
        action = "abort",
        call = call
      )
    }
    return(invisible(NULL))
  }
  declared <- tolower(model$name[typ %in% c("coefficient", "variable")])
  byele <- !is.na(model$qualifier_list[reads]) &
    grepl("by_elements", model$qualifier_list[reads], ignore.case = TRUE)
  tgt_all <- tolower(model$name[reads])

  bad_targets <- unique(tgt_all[byele & !tgt_all %in% map_names])
  if (length(bad_targets) > 0L) {
    .cli_action(model_err$byele_nonmap,
      action = "abort",
      call = call
    )
  }
  bad_targets <- unique(tgt_all[!byele & tgt_all %in% map_names])
  if (length(bad_targets) > 0L) {
    .cli_action(model_err$map_read_plain,
      action = "abort",
      call = call
    )
  }
  bad_maps <- unique(map_names[!map_names %in% tgt_all[byele]])
  if (length(bad_maps) > 0L) {
    .cli_action(model_err$map_read_missing,
      action = "abort",
      call = call
    )
  }

  ord_reads <- reads[!model$postsim[reads] & !byele]
  tgt <- tolower(model$name[ord_reads])
  undecl <- !is.na(tgt) & nzchar(tgt) & !tgt %in% declared
  if (any(undecl)) {
    bad_targets <- unique(tgt[undecl])
    .cli_action(model_err$read_undeclared,
      action = "abort",
      call = call
    )
  }
  return(invisible(NULL))
}
