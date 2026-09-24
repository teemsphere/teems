# load the single-file ORANI-G database at full resolution
dat <- ems_data(
  dat_input = dat_input
)

# the long-run closure ships beside the short-run one: rates of return
# fix capital, employment fixes the wage shifter, stock and balance-of-
# trade rules replace their shifters, and investment follows profits
# (ENDOGINV) or aggregate investment (EXOGINV), as oranigLR.CMF sets up
# through its swaps
longrun_closure <- file.path(dirname(closure_file), "ORANI-G-LR.cls")

# parse the model Tablo file and load the long-run closure
model <- ems_model(
  model_file = model_file,
  closure_file = longrun_closure
)

# the simulation distributed with the model (oranigLR.CMF): 1 per cent
# growth in the labour force
labour_growth <- ems_uniform_shock(
  var = "employ_i",
  value = 1
)

# validate inputs, write solver files, and return the CMF path
cmf_path <- ems_deploy(
  .data = dat,
  model = model,
  shock = labour_growth
)

# run the Docker-based solver and parse results, with the command
# file's step schedule
outputs <- ems_solve(
  cmf_path = cmf_path,
  matrix_method = "LU",
  solution_method = "Gragg",
  steps = c(2L, 4L, 6L)
)

# checks
# multi-step solutions carry rounding, so values are compared within a tolerance
shk <- abs(outputs$dat$employ_i$Value - 1) < 1e-6
len_check <- length(shk) == nrow(outputs$dat$employ_i)
# with rates of return fixed, capital grows with the labour force
capital_check <- outputs$dat$x1cap_i$Value > 0
gdp_check <- outputs$dat$x0gdpexp$Value > 0
# rates of return are exogenous and unshocked
gret_check <- all(abs(outputs$dat$gret$Value) < 1e-6)
checks <- c(shk, len_check, capital_check, gdp_check, gret_check)
