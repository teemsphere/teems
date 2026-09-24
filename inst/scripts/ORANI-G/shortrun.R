# load the single-file ORANI-G database at full resolution
dat <- ems_data(
  dat_input = dat_input
)

# parse the model Tablo file and load the short-run closure
model <- ems_model(
  model_file = model_file,
  closure_file = closure_file
)

# the simulation distributed with the model (oranigSR.CMF): a 5 per cent
# cut in the real wage under the DPSV short-run closure, solved by the
# one-step Johansen method as the command file does
wage_cut <- ems_uniform_shock(
  var = "realwage",
  value = -5
)

# validate inputs, write solver files, and return the CMF path
cmf_path <- ems_deploy(
  .data = dat,
  model = model,
  shock = wage_cut
)

# run the Docker-based solver and parse results
outputs <- ems_solve(
  cmf_path = cmf_path,
  matrix_method = "LU",
  solution_method = "Johansen"
)

# checks
shk <- abs(outputs$dat$realwage$Value + 5) < 1e-6
len_check <- length(shk) == nrow(outputs$dat$realwage)
# a cheaper wage raises employment and real GDP
employ_check <- outputs$dat$employ_i$Value > 0
gdp_check <- outputs$dat$x0gdpexp$Value > 0
# real GDP from the income side equals the expenditure side, and the
# expenditure-side decomposition adds up to it
gdp_sides <- abs(outputs$dat$x0gdpinc$Value - outputs$dat$x0gdpexp$Value) < 1e-5
gdp_decomp <- abs(sum(outputs$dat$contGDPexp$Value) - outputs$dat$x0gdpexp$Value) < 1e-5
checks <- c(shk, len_check, employ_check, gdp_check, gdp_sides, gdp_decomp)
