# load GTAP HAR files, apply set mappings, and aggregate data
dat <- ems_data(
  dat_input = dat_input,
  par_input = par_input,
  set_input = set_input,
  REG = "big3",
  PROD_COMM = "macro_sector",
  ENDW_COMM = "labor_agg"
)

# parse the model Tablo file and load the closure
model <- ems_model(
  model_file = model_file,
  closure_file = closure_file
)

# define a uniform shock on a subset of qfd elements
partial <- ems_uniform_shock(
  var = "qfd",
  REGs = "usa",
  PROD_COMMj = "crops",
  value = -1
)

# prepare a partial closure swap making qfd subset exogenous
qfd <- ems_swap(
  var = "qfd",
  REGs = "usa",
  PROD_COMMj = "crops"
)

# prepare a partial closure swap making tfd subset endogenous
tfd <- ems_swap(
  var = "tfd",
  REGr = "usa",
  PROD_COMMj = "crops"
)


# validate inputs, write solver files, and return the CMF path
cmf_path <- ems_deploy(
  .data = dat,
  model = model,
  shock = partial,
  swap_in = qfd,
  swap_out = tfd
)

# run the Docker-based solver and parse results
outputs <- ems_solve(
  cmf_path = cmf_path,
  matrix_method = "LU",
  solution_method = "Johansen"
)

# checks
exo_shk <- outputs$dat$qfd[REGs == "usa" & PROD_COMMj == "crops"]$Value == -1
endo1 <- outputs$dat$qfd[!(REGs == "usa" & PROD_COMMj == "crops")]$Value != 0
endo2 <- outputs$dat$tfd[REGr == "usa" & PROD_COMMj == "crops"]$Value != 0
exo_null <- outputs$dat$tfd[!(REGr == "usa" & PROD_COMMj == "crops")]$Value == 0
qfd_len_check <- (length(exo_shk) + length(endo1)) == nrow(outputs$dat$qfd)
tfd_len_check <- (length(endo2) + length(exo_null)) == nrow(outputs$dat$tfd)
checks <- c(exo_shk, endo1, endo2, exo_null, qfd_len_check, tfd_len_check)
