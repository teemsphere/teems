# load GTAP HAR files, apply set mappings, and aggregate data
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

# parse the model Tablo file and load the closure
model <- ems_model(
  model_file = model_file,
  closure_file = closure_file
)

# define a uniform shock on a subset of aoall elements at a specific timestep
partial <- ems_uniform_shock(
  var = "aoall",
  REGr = "chn",
  PROD_COMMj = "crops",
  Year = year,
  value = -1
)

# set the output subdirectory name within write_dir
ems_option_set(write_sub_dir = "part_uniform_year")

# validate inputs, write solver files, and return the CMF path
cmf_path <- ems_deploy(
  write_dir = write_dir,
  .data = .data,
  model = model,
  shock = partial
)

# run the Docker-based solver and parse results
outputs <- ems_solve(
  cmf_path = cmf_path,
  matrix_method = "LU",
  solution_method = "Johansen"
)

# checks
exo_shk <- outputs$dat$aoall[REGr == "chn" & PROD_COMMj == "crops" & Year == year]$Value == -1
exo_null <- outputs$dat$aoall[REGr != "chn" & PROD_COMMj != "crops" & Year != year]$Value == 0
checks <- c(exo_shk, exo_null)
