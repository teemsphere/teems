# load GTAP HAR files, apply set mappings, and aggregate data
time_steps <- c(0, 1, 2)
dat <- ems_data(
  dat_input = dat_input,
  par_input = par_input,
  set_input = set_input,
  REG = "big3",
  ACTS = "macro_sector",
  ENDW = "labor_agg",
  time_steps = time_steps
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
  ACTSa = "crops",
  Year = year,
  value = -1
)

# validate inputs, write solver files, and return the CMF path
cmf_path <- ems_deploy(
  .data = dat,
  model = model,
  shock = partial
)

# run the Docker-based solver and parse results
outputs <- ems_solve(
  cmf_path = cmf_path,
  matrix_method = "SBBD",
  n_tasks = 2L,
  solution_method = "Gragg"
)

# checks
# multi-step solutions carry rounding, so values are compared within a tolerance
exo_shk <- abs(outputs$dat$aoall[REGr == "chn" & ACTSa == "crops" & Year == year]$Value + 1) < 1e-6
exo_null <- abs(outputs$dat$aoall[!(REGr == "chn" & ACTSa == "crops" & Year == year)]$Value) < 1e-6
len_check <- (length(exo_shk) + length(exo_null)) == nrow(outputs$dat$aoall)
checks <- c(exo_shk, exo_null, len_check)
