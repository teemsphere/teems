build_set_err <- function() {
  list(
    missing_data = "There is no loaded set data which corresponds to the loaded set map: {.field {map_name}}.",
    invalid_user_input = "The set mapping loaded for {.field {map_name}} does not contain both an origin element column and a mapping column.",
    missing_ele_mapping = "The set mapping loaded for {.field {map_name}} is missing mappings for {.val {missing_ele}}.",
    while_loop = "Construction of dependent sets has failed on: {null_sets}.",
    invalid_plus = "The set operator `+` was used where there are overlapping set elements {.field {d}}, violating the condition that the sets be disjoint."
  )
}

build_set_wrn <- function() {
  list(
    invalid_user_input = "The set mapping loaded for {.field {map_name}} contains more than 2 columns. Only the first (origin element) and second (mapped element) columns will be utilized."
  )
}
