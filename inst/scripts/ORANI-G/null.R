# load the single-file ORANI-G database at full resolution
dat <- ems_data(
  dat_input = dat_input
)

# parse the model Tablo file and load the short-run closure
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
