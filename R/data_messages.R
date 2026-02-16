#' @importFrom rlang current_env
#'
#' @keywords internal
#' @noRd
.inform_metadata <- function(metadata) {
  if (.o_verbose()) {
    list2env(
      metadata,
      rlang::current_env()
    )
    .cli_action(data_info$dat,
      action = rep("inform", 3)
    )
  }
}

#' @importFrom utils packageVersion
#' 
#' @noRd
#' @keywords internal
.check_database_version <- function(vetted,
                                    provided,
                                    call,
                                    quiet) {
  if (!provided %in% vetted) {
    teems_version <- utils::packageVersion("teems")
    .cli_action(data_wrn$db_version,
                action = c("warn", "inform"),
                call = call
    )
  }
}
