# load GTAP HAR files, apply set mappings, and aggregate data
dat <- ems_data(
  dat_input = dat_input,
  par_input = par_input,
  set_input = set_input,
  REG = "big3",
  PROD_COMM = "macro_sector",
  ENDW_COMM = "labor_agg"
)

# parse the model Tablo file and load the closure
model <- ems_model(
  model_file = model_file,
  closure_file = closure_file
)

# define a uniform numeraire shock on pfactwld
numeraire <- ems_uniform_shock(
  var = "pfactwld",
  value = 1
)


# validate inputs, write solver files, and return the CMF path
cmf_path <- ems_deploy(
  .data = dat,
  model = model,
  shock = numeraire
)

# run the Docker-based solver and parse results
outputs <- ems_solve(
  cmf_path = cmf_path,
  matrix_method = "LU",
  solution_method = "Johansen"
)

# checks
shk <- outputs$dat$pfactwld$Value == 1
len_check <- length(shk) == nrow(outputs$dat$pfactwld)
checks <- c(shk, len_check)
