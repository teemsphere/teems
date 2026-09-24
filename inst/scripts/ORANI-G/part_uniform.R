# load the single-file ORANI-G database at full resolution
dat <- ems_data(
  dat_input = dat_input
)

# parse the model Tablo file and load the short-run closure
model <- ems_model(
  model_file = model_file,
  closure_file = closure_file
)

# define a uniform shock on a subset of a1cap elements: capital-
# augmenting technical change in one industry
partial <- ems_uniform_shock(
  var = "a1cap",
  INDi = "Mining",
  value = -2
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
  solution_method = "Gragg"
)

# checks
# multi-step solutions carry rounding, so values are compared within a tolerance
a1cap <- outputs$dat$a1cap
shk <- abs(a1cap[a1cap$IND == "mining", ]$Value + 2) < 1e-6
rest <- abs(a1cap[a1cap$IND != "mining", ]$Value) < 1e-6
len_check <- length(shk) + length(rest) == nrow(a1cap)
checks <- c(shk, rest, len_check)
