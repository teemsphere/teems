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

# define a uniform percentage change shock across all pop elements
numeraire <- ems_uniform_shock(
  var = "pop",
  value = 1
)

# set the output subdirectory name within write_dir
ems_option_set(write_sub_dir = "full_uniform")

# validate inputs, write solver files, and return the CMF path
cmf_path <- ems_deploy(
  write_dir = write_dir,
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
shk       <- outputs$dat$pop$Value == 1
len_check <- length(shk) == nrow(outputs$dat$pop)
checks    <- c(shk, len_check)
