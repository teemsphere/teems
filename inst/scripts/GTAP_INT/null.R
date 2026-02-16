.data <- ems_data(
  dat_input = dat_input,
  par_input = par_input,
  set_input = set_input,
  REG = "big3",
  TRAD_COMM = "macro_sector",
  ENDW_COMM = "labor_agg",
  time_steps = c(0, 1, 2),
  target_format = target_format
)

model <- ems_model(
  model_input = model_input,
  closure_file = closure_file
)

ems_option_set(write_sub_dir = "null")

cmf_path <- ems_deploy(
  write_dir = write_dir,
  .data = .data,
  model = model
)

outputs <- ems_solve(
  cmf_path = cmf_path,
  matrix_method = "LU",
  solution_method = "Johansen"
)