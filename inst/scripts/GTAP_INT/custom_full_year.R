time_steps <- year + c(0, 1, 2, 3)

.data <- ems_data(
  dat_input = dat_input,
  par_input = par_input,
  set_input = set_input,
  REG = "big3",
  TRAD_COMM = "macro_sector",
  ENDW_COMM = "labor_agg",
  time_steps = time_steps,
  target_format = target_format
)

model <- ems_model(
  model_input = model_input,
  closure_file = closure_file
)

REG <- c("chn", "usa", "row")
ENDW_COMM <- c("labor", "capital", "natlres", "land")
TRAD_COMM <- c("svces", "food", "crops", "mnfcs", "livestock")
PROD_COMM <- c("svces", "food", "crops", "mnfcs", "livestock", "zcgds")
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

pop_shk <- ems_shock(
  var = "pop",
  type = "custom",
  input = pop
)

aoall_shk <- ems_shock(
  var = "aoall",
  type = "custom",
  input = aoall
)

afeall_shk <- ems_shock(
  var = "afeall",
  type = "custom",
  input = afeall
)

atall_shk <- ems_shock(
  var = "atall",
  type = "custom",
  input = atall
)

ems_option_set(write_sub_dir = "custom_full_year")

cmf_path <- ems_deploy(
  write_dir = write_dir,
  .data = .data,
  model = model,
  shock = list(pop_shk, aoall_shk, afeall_shk, atall_shk)
)

outputs <- ems_solve(
  cmf_path = cmf_path,
  matrix_method = "LU",
  solution_method = "Johansen"
)

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