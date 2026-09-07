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

# validate inputs, write solver files, and return the CMF path
cmf_path <- ems_deploy(
  .data = dat,
  model = model
)

# run the Docker-based solver and parse results
outputs <- ems_solve(
  cmf_path = cmf_path,
  matrix_method = "LU",
  solution_method = "Johansen"
)

# checks
check <- all(unlist(lapply(
  outputs[outputs$type == "variable", ]$dat,
  \(d) {
    all(d$Value == 0)
  }
)))

# variables omitted by the model file's condensation statements leave the
# system and do not appear in the outputs
n_var <- nrow(model[which(model$type == "Variable" & !model$condense %in% "omit"), ])
n_coeff <- nrow(model[which(model$type == "Coefficient"), ])

var_check <- isTRUE(all.equal(n_var, nrow(outputs[which(outputs$type == "variable"), ])))
# PostSim report coefficients return with their own output type
coeff_check <- isTRUE(all.equal(n_coeff, nrow(outputs[which(outputs$type %in% c("coefficient", "postsim")), ])))
