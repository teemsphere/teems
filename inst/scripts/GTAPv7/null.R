# load GTAP HAR files, apply set mappings, and aggregate data
.data <- ems_data(
  dat_input = dat_input,
  par_input = par_input,
  set_input = set_input,
  REG = "big3",
  ACTS = "macro_sector",
  ENDW = "labor_agg",
  target_format = target_format
)

# parse the model Tablo file and load the closure
model <- ems_model(
  model_file = model_file,
  closure_file = closure_file
)

# set the output subdirectory name within write_dir
ems_option_set(write_sub_dir = "null")

# validate inputs, write solver files, and return the CMF path
cmf_path <- ems_deploy(
  write_dir = write_dir,
  .data = .data,
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

n_var <- nrow(model[which(model$type == "Variable"),])
n_coeff <- nrow(model[which(model$type == "Coefficient"),])

var_check <- isTRUE(all.equal(n_var, nrow(outputs[which(outputs$type == "variable"),])))
coeff_check <- isTRUE(all.equal(n_coeff, nrow(outputs[which(outputs$type == "coefficient"),])))
