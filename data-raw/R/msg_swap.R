build_swap_err <- function() {
  list(
    no_var = "The variable {.val {var_name}} designated for a swap is not found within the model Tablo file provided.",
    invalid_set = c(
      "The swap set {.val {non_exist_set}} is not associated with the variable {.val {var_name}}.",
      "Note that set designations within {.pkg teems} are comprised of the variable-specific uppercase set name and lowercase index.",
      "For {.val {var_name}} these include: {.field {ls_mixed}}."
    ),
    invalid_comp = c(
      "Elements or subsets designated for a swap are not part of the set {.field {nm}}: {invalid_comp}.",
      "Valid elements include {.val {valid_ele}}.",
      "Valid subsets include {.field {valid_subsets}}."
    ),
    invalid_tup = "{n_invalid_tuples} tuple{?s} designated to be swapped out on {.val {var_name}} are not identified as exogenous: {.field {invalid_tuples}}.",
    pre_overlap_ele = "{n_overlap} tuple{?s} for {.val {e}} in the supplied pre-swap closure has multiple entries: {overlap}.",
    overlap_ele = "The proposed swap on {.val {var_name}} results in duplicate closure entries: {.field {overlap}}.",
    no_var_cls = "There is no closure entry for the selected swap-out variable {.val {var_name}}.",
    invalid_full = "A full swap on the variable {.val {var_name}} is not possible as the variable is not fully exogenous."
  )
}
