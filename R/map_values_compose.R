#' @importFrom utils head
#' @keywords internal
#' @noRd
.compose_map_values <- function(vals,
                                dom_orig,
                                dom_map,
                                cod_map,
                                agg_ele,
                                map_name,
                                dom,
                                cod,
                                header,
                                call) {
  keep <- dom_orig %in% dom_map$origin
  dom_orig <- dom_orig[keep]
  vals <- vals[keep]

  bad_vals <- setdiff(unique(vals), cod_map$origin)
  if (length(bad_vals) > 0L) {
    .cli_action(deploy_err$map_data_ele,
      action = "abort",
      call = call
    )
  }

  vals_agg <- cod_map$mapping[match(vals, cod_map$origin)]
  dom_agg <- dom_map$mapping[match(dom_orig, dom_map$origin)]

  composed <- character(length(agg_ele))
  for (j in seq_along(agg_ele)) {
    members <- dom_agg == agg_ele[j]
    u <- unique(vals_agg[members])
    if (length(u) != 1L) {
      split_detail <- vapply(u, \(vv) {
        src <- dom_orig[members][vals_agg[members] == vv]
        shown <- utils::head(src, 3L)
        more <- length(src) - length(shown)
        paste0(
          vv, " from ", paste(shown, collapse = ", "),
          if (more > 0L) {
            paste0(" and ", more, " more")
          } else {
            ""
          }
        )
      }, character(1))
      split_detail <- paste(split_detail, collapse = "; ")
      agg_ele <- agg_ele[j]
      .cli_action(deploy_err$map_agg_split,
        action = c("abort", "inform"),
        call = call
      )
    }
    composed[j] <- u
  }
  return(composed)
}
