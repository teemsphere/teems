build_deploy_err <- function() {
  list(
    missing_header = "The following headers are designated as read-in but are missing from the loaded data: {.val {missing_headers}}.",
    while_loop = "Construction of dependent sets has failed on: {null_sets}.",
    missing_mapping = "Some model sets that are read as headers are missing mappings: {.field {m_map}}.",
    nonreq_tsteps = "{.arg time_steps} have been provided yet no intertemporal sets have been detected in the provided model. See {.fun teems::ems_data}.",
    missing_tsteps = "{.arg time_steps} have not been provided to an intertemporal model. See {.fun teems::ems_data}.",
    data_set_mismatch = "The expected number of data entries on {.field {class(dt)[1]}} ({.val {expected}}) is not equal to the number found ({.val {nrow(dt)}}).",
    invalid_write_dir = "The path provided for {.arg write_dir}, {.path {write_dir}}, does not exist and could not be created. Ensure the parent directory exists.",
    write_dir_not_writable = "The directory {.path {write_dir}} exists but is not writable.",
    invalid_plus = "The set operator `+` was used where there are overlapping set elements {.field {d}}, violating the condition that the sets be disjoint.",
    agg_missing_tup = "{tup$n} tuple{?s} in the provided input file for {.val {nme}} were missing: {.field {tup$missing}}.",
    ele_invalid = "The closure entry tuple {.field {cls_entry}} is invalid under the current set mapping.",
    mixed_invalid = "{n_invalid_entries} closure entry element{?s} in {.field {cls_entry}} do not belong to the respective variable sets: {invalid_entries}.",
    subset_invalid = "Some entries from {.field {cls_entry}} do not belong to the respective variables sets indicating an invalid subset."
  )
}
