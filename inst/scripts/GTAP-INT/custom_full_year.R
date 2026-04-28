time_steps <- year + c(0, 1, 2, 3)

# load GTAP HAR files, apply set mappings, and aggregate data
.data <- ems_data(
  dat_input = dat_input,
  par_input = par_input,
  set_input = set_input,
  REG = "big3",
  PROD_COMM = "macro_sector",
  ENDW_COMM = "labor_agg",
  time_steps = time_steps,
  target_format = target_format
)

# parse the model Tablo file and load the closure
model <- ems_model(
  model_file = model_file,
  closure_file = closure_file
)

REG <- c("chn", "usa", "row")
ENDW_COMM <- c("labor", "capital", "natlres", "land")
TRAD_COMM <- c("svces", "food", "crops", "mnfcs", "livestock")
PROD_COMM <- c("svces", "food", "crops", "mnfcs", "livestock", "cgds")
MARG_COMM <- "svces"

# 2D
pop <- expand.grid(
  REGr = REG,
  Year = time_steps,
  stringsAsFactors = FALSE
)

pop <- pop[do.call(order, pop), ]
pop$Value <- runif(nrow(pop))

# 3D
aoall <- expand.grid(
  PROD_COMMj = PROD_COMM,
  REGr = REG,
  Year = time_steps,
  stringsAsFactors = FALSE
)

aoall <- aoall[do.call(order, aoall), ]
aoall$Value <- runif(nrow(aoall))

# 4D
afeall <- expand.grid(
  ENDW_COMMi = ENDW_COMM,
  PROD_COMMj = PROD_COMM,
  REGr = REG,
  Year = time_steps,
  stringsAsFactors = FALSE
)

afeall <- afeall[do.call(order, afeall), ]
afeall$Value <- runif(nrow(afeall))

# 5D
atall <- expand.grid(
  MARG_COMMm = MARG_COMM,
  TRAD_COMMi = TRAD_COMM,
  REGr = REG,
  REGs = REG,
  Year = time_steps,
  stringsAsFactors = FALSE
)

atall <- atall[do.call(order, atall), ]
atall$Value <- runif(nrow(atall))

# define a custom percentage change shock over all pop elements using chronological year indexing
pop_shk <- ems_custom_shock(
  var = "pop",
  input = pop
)

# define a custom percentage change shock over all aoall elements using chronological year indexing
aoall_shk <- ems_custom_shock(
  var = "aoall",
  input = aoall
)

# define a custom percentage change shock over all afeall elements using chronological year indexing
afeall_shk <- ems_custom_shock(
  var = "afeall",
  input = afeall
)

# define a custom percentage change shock over all atall elements using chronological year indexing
atall_shk <- ems_custom_shock(
  var = "atall",
  input = atall
)

# set the output subdirectory name within write_dir
ems_option_set(write_sub_dir = "custom_full_year")

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
t_tbl <- data.frame(
  Year = unique(pop$Year),
  ALLTIMEt = unique(outputs$dat$pop$ALLTIMEt)
)

pop$ALLTIMEt <- t_tbl$ALLTIMEt[match(pop$Year, t_tbl$Year)]
pop <- pop[, match(colnames(pop), colnames(outputs$dat$pop))]
aoall$ALLTIMEt <- t_tbl$ALLTIMEt[match(aoall$Year, t_tbl$Year)]
aoall <- aoall[, match(colnames(aoall), colnames(outputs$dat$aoall))]
afeall$ALLTIMEt <- t_tbl$ALLTIMEt[match(afeall$Year, t_tbl$Year)]
afeall <- afeall[, match(colnames(afeall), colnames(outputs$dat$afeall))]
atall$ALLTIMEt <- t_tbl$ALLTIMEt[match(atall$Year, t_tbl$Year)]
atall <- atall[, match(colnames(atall), colnames(outputs$dat$atall))]

pop_check <- isTRUE(
  all.equal(pop,
    outputs$dat$pop,
    check.attributes = FALSE,
    tolerance = 1e-6
  )
)

aoall_check <- isTRUE(
  all.equal(aoall,
    outputs$dat$aoall,
    check.attributes = FALSE,
    tolerance = 1e-6
  )
)

afeall_check <- isTRUE(
  all.equal(afeall,
    outputs$dat$afeall,
    check.attributes = FALSE,
    tolerance = 1e-6
  )
)

atall_check <- isTRUE(
  all.equal(atall,
    outputs$dat$atall,
    check.attributes = FALSE,
    tolerance = 1e-6
  )
)

checks <- c(pop_check, aoall_check, afeall_check, atall_check)
