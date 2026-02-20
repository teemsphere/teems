.data <- ems_data(
  dat_input = dat_input,
  par_input = par_input,
  set_input = set_input,
  REG = "big3",
  COMM = "macro_sector",
  ACTS = "macro_sector",
  ENDW = "labor_agg",
  time_steps = c(0, 1, 2),
  target_format = target_format
)

model <- ems_model(
  model_input = model_input,
  closure_file = closure_file
)

partial <- ems_uniform_shock(
  var = "qfd",
  REGr = "usa",
  ACTSa = "crops",
  value = -1
)

full <- ems_uniform_shock(
  var = "yp",
  value = 0.1
)

qfd <- ems_swap(
  var = "qfd",
  REGr = "usa",
  ACTSa = "crops"
)

tfd <- ems_swap(
  var = "tfd",
  REGr = "usa",
  ACTSa = "crops"
)

ems_option_set(write_sub_dir = "part_uniform_part_swap_mixed")

cmf_path <- ems_deploy(
  write_dir = write_dir,
  .data = .data,
  model = model,
  shock = list(partial, full),
  swap_in = list(qfd, "yp"),
  swap_out = list(tfd, "dppriv")
)

outputs <- ems_solve(
  cmf_path = cmf_path,
  matrix_method = "LU",
  solution_method = "Johansen"
)