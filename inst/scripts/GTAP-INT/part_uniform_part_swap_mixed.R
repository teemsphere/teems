# load GTAP HAR files, apply set mappings, and aggregate data
time_steps <- c(0, 1, 2)
dat <- ems_data(
  dat_input = dat_input,
  par_input = par_input,
  set_input = set_input,
  REG = "big3",
  PROD_COMM = "macro_sector",
  ENDW_COMM = "labor_agg",
  time_steps = time_steps
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

# define a uniform shock across all yp elements
full <- ems_uniform_shock(
  var = "yp",
  value = 0.1
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

# set the output subdirectory name within write_dir
ems_option_set(write_sub_dir = "part_uniform_part_swap_mixed")

# validate inputs, write solver files, with mixed partial and full variable swaps
cmf_path <- ems_deploy(
  write_dir = write_dir,
  .data = dat,
  model = model,
  shock = list(partial, full),
  swap_in = list(qfd, "yp"),
  swap_out = list(tfd, "dppriv")
)

# run the Docker-based solver and parse results
outputs <- ems_solve(
  cmf_path = cmf_path,
  matrix_method = "LU",
  solution_method = "Johansen"
)

# checks
exo_shk1         <- outputs$dat$qfd[REGs == "usa" & PROD_COMMj == "crops"]$Value == -1
endo1            <- outputs$dat$qfd[!(REGs == "usa" & PROD_COMMj == "crops")]$Value != 0
endo2            <- outputs$dat$tfd[REGr == "usa" & PROD_COMMj == "crops"]$Value != 0
exo_null         <- outputs$dat$tfd[!(REGr == "usa" & PROD_COMMj == "crops")]$Value == 0
endo3            <- outputs$dat$dppriv$Value != 0
exo_shk2         <- abs(outputs$dat$yp$Value - 0.1) < .Machine$double.eps^0.5
qfd_len_check    <- (length(exo_shk1) + length(endo1)) == nrow(outputs$dat$qfd)
tfd_len_check    <- (length(endo2) + length(exo_null)) == nrow(outputs$dat$tfd)
dppriv_len_check <- length(endo3) == nrow(outputs$dat$dppriv)
yp_len_check     <- length(exo_shk2) == nrow(outputs$dat$yp)
checks <- c(exo_shk1, endo1, endo2, exo_null, endo3, exo_shk2,
            qfd_len_check, tfd_len_check, dppriv_len_check, yp_len_check)
