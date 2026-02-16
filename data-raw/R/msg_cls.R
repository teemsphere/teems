build_cls_err <- function() {
  list(
    no_var = "{l_var} variable{?s} from the closure file not present in the Tablo file: {.val {var_discrepancy}}.",
    no_cls = "A closure file must be provided when a user-provided Tablo file has been supplied.",
    entry_type = "The following closure entries have not been classified properly: {invalid_entry}.",
    ele_invalid = "The closure entry tuple {.field {cls_entry}} is invalid under the current set mapping.",
    mixed_invalid = "{n_invalid_entries} closure entry element{?s} in {.field {cls_entry}} do not belong to the respective variable sets: {invalid_entries}.",
    subset_invalid = "Some entries from {.field {cls_entry}} do not belong to the respective variables sets indicating an invalid subset.",
    invalid_full = "A full swap on the variable {.val {var_name}} is not possible as the variable is not fully exogenous.",
    no_var_cls = "There is no closure entry for the selected swap-out variable {.val {var_name}}.",
    missing_specification = "The closure provided must contain both an \"Exogenous\" entry and a \"Rest Endogenous\" entry. Note that the inverse approach is not currently supported."
  )
}
