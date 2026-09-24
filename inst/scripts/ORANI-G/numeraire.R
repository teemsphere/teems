# load the single-file ORANI-G database at full resolution
dat <- ems_data(
  dat_input = dat_input
)

# parse the model Tablo file and load the short-run closure
model <- ems_model(
  model_file = model_file,
  closure_file = closure_file
)

# the exchange rate phi is the numeraire of the short-run closure: a
# uniform shock to it moves every nominal variable by the same
# percentage and leaves the real variables untouched
numeraire <- ems_uniform_shock(
  var = "phi",
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
  solution_method = "Gragg"
)

# checks
# multi-step solutions carry rounding, so values are compared within a tolerance
shk <- abs(outputs$dat$phi$Value - 1) < 1e-6
len_check <- length(shk) == nrow(outputs$dat$phi)
# nominal homogeneity: the consumer price index and the average wage
# follow the numeraire, real GDP does not move
nominal_check <- abs(outputs$dat$p3tot$Value - 1) < 1e-5 &&
  abs(outputs$dat$p1lab_io$Value - 1) < 1e-5
real_check <- abs(outputs$dat$x0gdpexp$Value) < 1e-5
checks <- c(shk, len_check, nominal_check, real_check)
