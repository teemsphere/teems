time_steps <- c(0, 1, 2, 3)

# load GTAP HAR files, apply set mappings, and aggregate data
.data <- ems_data(
  dat_input = dat_input,
  par_input = par_input,
  set_input = set_input,
  REG = "big3",
  ACTS = "macro_sector",
  ENDW = "labor_agg",
  time_steps = time_steps,
  target_format = target_format
)

# parse the model Tablo file and load the closure
model <- ems_model(
  model_file = model_file,
  closure_file = closure_file
)

REG <- c("chn", "usa", "row")
ENDW <- c("labor", "capital", "natlres", "land")
COMM <- c("svces", "food", "crops", "mnfcs", "livestock")
ACTS <- COMM
MARG <- "svces"
ALLTIME <- seq(0, length(time_steps) - 1)

# 2D
pop <- expand.grid(
  REGr = REG,
  ALLTIMEt = ALLTIME,
  stringsAsFactors = FALSE
)

pop <- pop[do.call(order, pop), ]
pop$Value <- runif(nrow(pop))
pop_csv <- tempfile(fileext = ".csv")
write.csv(pop, pop_csv, row.names = FALSE)

# 3D
aoall <- expand.grid(
  ACTSa = ACTS,
  REGr = REG,
  ALLTIMEt = ALLTIME,
  stringsAsFactors = FALSE
)

aoall <- aoall[do.call(order, aoall), ]
aoall$Value <- runif(nrow(aoall))
aoall_csv <- tempfile(fileext = ".csv")
write.csv(aoall, aoall_csv, row.names = FALSE)

# 4D
afeall <- expand.grid(
  ENDWe = ENDW,
  ACTSa = ACTS,
  REGr = REG,
  ALLTIMEt = ALLTIME,
  stringsAsFactors = FALSE
)

afeall <- afeall[do.call(order, afeall), ]
afeall$Value <- runif(nrow(afeall))
afeall_csv <- tempfile(fileext = ".csv")
write.csv(afeall, afeall_csv, row.names = FALSE)

# 5D
atall <- expand.grid(
  MARGm = MARG,
  COMMc = COMM,
  REGs = REG,
  REGd = REG,
  ALLTIMEt = ALLTIME,
  stringsAsFactors = FALSE
)

atall <- atall[do.call(order, atall), ]
atall$Value <- runif(nrow(atall))
atall_csv <- tempfile(fileext = ".csv")
write.csv(atall, atall_csv, row.names = FALSE)

# define a custom percentage change shock over all pop elements, loaded from a CSV file
pop_shk <- ems_custom_shock(
  var = "pop",
  input = pop_csv
)

# define a custom percentage change shock over all aoall elements, loaded from a CSV file
aoall_shk <- ems_custom_shock(
  var = "aoall",
  input = aoall_csv
)

# define a custom percentage change shock over all afeall elements, loaded from a CSV file
afeall_shk <- ems_custom_shock(
  var = "afeall",
  input = afeall_csv
)

# define a custom percentage change shock over all atall elements, loaded from a CSV file
atall_shk <- ems_custom_shock(
  var = "atall",
  input = atall_csv
)

# set the output subdirectory name within write_dir
ems_option_set(write_sub_dir = "custom_full")

# validate inputs, write solver files, and return the CMF path
cmf_path <- ems_deploy(
  write_dir = write_dir,
  .data = .data,
  model = model,
  shock = list(pop_shk, aoall_shk, afeall_shk, atall_shk)
)

# run the Docker-based solver and parse results
outputs <- ems_solve(
  cmf_path = cmf_path,
  matrix_method = "LU",
  solution_method = "Johansen"
)

# checks
pop_check <- isTRUE(all.equal(pop,
  outputs$dat$pop[, !"Year"],
  check.attributes = FALSE,
  tolerance = 1e-6
))

aoall_check <- isTRUE(all.equal(aoall,
  outputs$dat$aoall[, !"Year"],
  check.attributes = FALSE,
  tolerance = 1e-6
))

afeall_check <- isTRUE(all.equal(afeall,
  outputs$dat$afeall[, !"Year"],
  check.attributes = FALSE,
  tolerance = 1e-6
))

atall_check <- isTRUE(all.equal(atall,
  outputs$dat$atall[, !"Year"],
  check.attributes = FALSE,
  tolerance = 1e-6
))

checks <- c(pop_check, aoall_check, afeall_check, atall_check)
