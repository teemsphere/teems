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

partial <- ems_shock(
  var = "qfd",
  type = "uniform",
  REGs = "usa",
  PROD_COMMj = "crops",
  value = -1
)

qfd <- ems_swap(
  var = "qfd",
  REGs = "usa",
  PROD_COMMj = "crops"
)

tfd <- ems_swap(
  var = "tfd",
  REGr = "usa",
  PROD_COMMj = "crops"
)

ems_option_set(write_sub_dir = "part_uniform_part_swap")

cmf_path <- ems_deploy(
  write_dir = write_dir,
  .data = .data,
  model = model,
  shock = partial,
  swap_in = qfd,
  swap_out = tfd
)

outputs <- ems_solve(
  cmf_path = cmf_path,
  matrix_method = "LU",
  solution_method = "Johansen"
)