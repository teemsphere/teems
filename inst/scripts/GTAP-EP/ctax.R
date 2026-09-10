# load GTAP HAR files, apply set mappings, and aggregate data
# the power aggregation keeps transmission, base load and peak load
# apart, which the generation nest requires: a mapping that merges a
# base-load technology with a peak-load one makes EBL* and EPL* overlap
dat <- ems_data(
  dat_input = dat_input,
  par_input = par_input,
  set_input = set_input,
  REG = "big3",
  ACTS = "power"
)

# parse the model Tablo file and load the closure
model <- ems_model(
  model_file = model_file,
  closure_file = closure_file
)

# the carbon tax scenario distributed with the model: levy a nominal
# carbon tax on every trading bloc. The database ships one bloc per
# region, so REGTOBLOC carries the levy to each region in the bloc.
levy <- ems_uniform_shock(
  var = "del_nctaxb",
  value = 10
)

# the closure holds the real carbon tax rate exogenous; swap it for the
# nominal rate so the nominal rate can be shocked and the real rate is
# determined by the regional price index
nominal <- ems_swap(
  var = "del_nctaxb"
)

real <- ems_swap(
  var = "del_rctaxb"
)

# validate inputs, write solver files, and return the CMF path
cmf_path <- ems_deploy(
  .data = dat,
  model = model,
  shock = levy,
  swap_in = nominal,
  swap_out = real
)

# run the Docker-based solver and parse results; a carbon tax is a large
# shock, so the multi-step solution and the subintervals the distributed
# scenario uses, without which the linearization is not accurate enough
outputs <- ems_solve(
  cmf_path = cmf_path,
  matrix_method = "LU",
  solution_method = "Gragg",
  steps = c(2L, 4L, 6L),
  n_subintervals = 5L
)

# checks
# a shocked change variable is accumulated over the steps and
# subintervals rather than set directly, as it is under a single-step
# Johansen solution, so it carries the solution's own error
shk <- abs(outputs$dat$del_nctaxb$Value - 10) < 1e-5
shk_len_check <- length(shk) == nrow(outputs$dat$del_nctaxb)
# the real rate is endogenous under the swap and deflates the nominal one
endo <- outputs$dat$del_rctaxb$Value != 0
endo_len_check <- length(endo) == nrow(outputs$dat$del_rctaxb)
checks <- c(shk, shk_len_check, endo, endo_len_check)
