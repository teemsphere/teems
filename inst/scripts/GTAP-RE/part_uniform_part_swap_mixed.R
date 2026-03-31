# load GTAP HAR files, apply set mappings, and aggregate data
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

# define a uniform shock across all yp elements
full <- ems_uniform_shock(
  var = "yp",
  value = 0.1
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
ems_option_set(write_sub_dir = "part_uniform_part_swap_mixed")

# validate inputs, write solver files, with mixed partial and full variable swaps
cmf_path <- ems_deploy(
  write_dir = write_dir,
  .data = .data,
  model = model,
  shock = list(partial, full),
  swap_in = list(qfd, "yp"),
  swap_out = list(tfd, "dppriv")
)

# run the Docker-based solver and parse results
outputs <- ems_solve(
  cmf_path = cmf_path,
  matrix_method = "LU",
  solution_method = "Johansen"
)

# checks
exo_shk1 <- outputs$dat$qfd[REGr == "usa" & ACTSa == "crops"]$Value == -1
endo1 <- outputs$dat$qfd[REGr != "usa" & ACTSa != "crops"]$Value != 0
endo2 <- outputs$dat$tfd[REGr == "usa" & ACTSa == "crops"]$Value != 0
exo_null <- outputs$dat$tfd[REGr == "usa" & ACTSa != "crops"]$Value == 0
endo3 <- outputs$dat$dppriv$Value != 0
exo_shk2 <- dplyr::near(outputs$dat$yp$Value, 0.1)
checks <- c(exo_shk1, endo1, endo2, exo_null, endo3, exo_shk2)
