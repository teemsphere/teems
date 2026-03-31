# load GTAP HAR files, apply set mappings, and aggregate data
.data <- ems_data(
  dat_input = dat_input,
  par_input = par_input,
  set_input = set_input,
  REG = "big3",
  COMM = "macro_sector",
  ACTS = "macro_sector",
  ENDW = "labor_agg",
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
  REGr = "usa",
  ACTSa = "crops",
  value = -1
)

# prepare a partial closure swap making qfd subset exogenous
qfd <- ems_swap(
  var = "qfd",
  REGr = "usa",
  ACTSa = "crops"
)

# prepare a partial closure swap making tfd subset endogenous
tfd <- ems_swap(
  var = "tfd",
  REGr = "usa",
  ACTSa = "crops"
)

# set the output subdirectory name within write_dir
ems_option_set(write_sub_dir = "part_uniform_part_swap")

# validate inputs, write solver files, and return the CMF path
cmf_path <- ems_deploy(
  write_dir = write_dir,
  .data = .data,
  model = model,
  shock = partial,
  swap_in = qfd,
  swap_out = tfd
)

# run the Docker-based solver and parse results
outputs <- ems_solve(
  cmf_path = cmf_path,
  matrix_method = "LU",
  solution_method = "Johansen"
)

# checks
exo_shk <- outputs$dat$qfd[REGr == "usa" & ACTSa == "crops"]$Value == -1
endo1 <- outputs$dat$qfd[REGr != "usa" & ACTSa != "crops"]$Value != 0
endo2 <- outputs$dat$tfd[REGr == "usa" & ACTSa == "crops"]$Value != 0
exo_null <- outputs$dat$tfd[REGr != "usa" & ACTSa != "crops"]$Value == 0
checks <- c(exo_shk, endo1, endo2, exo_null)
