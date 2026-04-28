# load GTAP HAR files, apply set mappings, and aggregate data
.data <- ems_data(
  dat_input = dat_input,
  par_input = par_input,
  set_input = set_input,
  REG = "big3",
  PROD_COMM = "macro_sector",
  ENDW_COMM = "labor_agg",
  target_format = target_format
)

# parse the model Tablo file and load the closure
model <- ems_model(
  model_file = model_file,
  closure_file = closure_file
)

# define a uniform shock on a subset of qfd elements
partial <- ems_uniform_shock(
  var = "qfd",
  REGs = "usa",
  PROD_COMMj = "crops",
  value = -1
)

# set the output subdirectory name within write_dir
ems_option_set(write_sub_dir = "part_uniform_full_swap")

# validate inputs, write solver files, with full variable swaps passed as strings
cmf_path <- ems_deploy(
  write_dir = write_dir,
  .data = .data,
  model = model,
  shock = partial,
  swap_in = "qfd",
  swap_out = "tfd"
)

# run the Docker-based solver and parse results
outputs <- ems_solve(
  cmf_path = cmf_path,
  matrix_method = "LU",
  solution_method = "Johansen"
)

# checks
exo_shk <- outputs$dat$qfd[REGs == "usa" & PROD_COMMj == "crops"]$Value == -1
exo_null <- outputs$dat$qfd[!(REGs == "usa" & PROD_COMMj == "crops")]$Value == 0
endo <- outputs$dat$tfd$Value != 0
checks <- c(exo_shk, exo_null, endo)
