# load GTAP HAR files, apply set mappings, and aggregate data
dat <- ems_data(
  dat_input = dat_input,
  par_input = par_input,
  set_input = set_input,
  REG = "big3",
  ACTS = "macro_sector",
  ENDW = "labor_agg"
)

# parse the model Tablo file and load the closure
model <- ems_model(
  model_file = model_file,
  closure_file = closure_file
)

# define a uniform shock on a subset of aoall elements
partial <- ems_uniform_shock(
  var = "aoall",
  REGr = "chn",
  ACTSa = "crops",
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
  matrix_method = "LU",
  solution_method = "Johansen"
)

# checks
shk <- outputs$dat$aoall[REGr == "chn" & ACTSa == "crops"]$Value == -1
null <- outputs$dat$aoall[!(REGr == "chn" & ACTSa == "crops")]$Value == 0
len_check <- (length(shk) + length(null)) == nrow(outputs$dat$aoall)
checks <- c(shk, null, len_check)
