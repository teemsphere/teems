# load GTAP HAR files, apply set mappings, and aggregate data
# the energy aggregation keeps the six energy commodities distinct, which
# the GTAP-E nest requires: an aggregation that merges them collapses the
# builder sets onto one element and leaves nothing to substitute between
dat <- ems_data(
  dat_input = dat_input,
  par_input = par_input,
  set_input = set_input,
  REG = "big3",
  ACTS = "energy"
)

# parse the model Tablo file and load the closure
model <- ems_model(
  model_file = model_file,
  closure_file = closure_file
)

# validate inputs, write solver files, and return the CMF path
cmf_path <- ems_deploy(
  .data = dat,
  model = model
)

# run the Docker-based solver and parse results
outputs <- ems_solve(
  cmf_path = cmf_path,
  matrix_method = "LU",
  solution_method = "Gragg"
)

# checks
# multi-step solutions carry rounding, so values are compared within a tolerance
check <- all(unlist(lapply(
  outputs[outputs$type == "variable", ]$dat,
  \(d) {
    all(abs(d$Value) < 1e-6)
  }
)))

n_var <- nrow(model[which(model$type == "Variable"), ])
n_coeff <- nrow(model[which(model$type == "Coefficient"), ])

var_check <- isTRUE(all.equal(n_var, nrow(outputs[which(outputs$type == "variable"), ])))
# PostSim report coefficients return with their own output type
coeff_check <- isTRUE(all.equal(n_coeff, nrow(outputs[which(outputs$type %in% c("coefficient", "postsim")), ])))
