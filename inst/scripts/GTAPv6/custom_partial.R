# load GTAP HAR files, apply set mappings, and aggregate data
.data <- ems_data(
  dat_input = dat_input,
  par_input = par_input,
  set_input = set_input,
  REG = "big3",
  TRAD_COMM = "macro_sector",
  ENDW_COMM = "labor_agg",
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
PROD_COMM <- c("svces", "food", "crops", "mnfcs", "livestock", "zcgds")
MARG_COMM <- "svces"

set.seed(42)

# 1D: remove 1 specific (REG) element — irregular, exercises element-by-element fallback
pop_full <- expand.grid(
  REGr = REG,
  stringsAsFactors = FALSE
)

pop_full$Value <- 0
pop_full <- pop_full[do.call(order, pop_full), ]
pop <- pop_full[pop_full$REGr != "chn", ]
pop$Value <- runif(nrow(pop))

# 3D: remove rows that break complete-group formation for every free_idx subset
aoall_full <- expand.grid(
  PROD_COMMj = PROD_COMM,
  REGr = REG,
  stringsAsFactors = FALSE
)
aoall_full$Value <- 0
aoall_full <- aoall_full[do.call(order, aoall_full), ]
aoall <- aoall_full[
  !(aoall_full$PROD_COMMj == "crops" & aoall_full$REGr == "usa") ,
]
aoall$Value <- runif(nrow(aoall))

# 4D: remove by two simultaneous dim conditions — breaks all potential complete groups
afeall_full <- expand.grid(
  ENDW_COMMi = ENDW_COMM,
  PROD_COMMj = PROD_COMM,
  REGr = REG,
  stringsAsFactors = FALSE
)
afeall_full$Value <- 0
afeall_full <- afeall_full[do.call(order, afeall_full), ]
afeall <- afeall_full[
  afeall_full$ENDW_COMMi != "natlres" & afeall_full$REGr != "row",
]
afeall$Value <- runif(nrow(afeall))

# 4D: deterministic removal — remove all (REGr=="chn", REGs=="usa") tuples
atall_full <- expand.grid(
  MARG_COMMm = MARG_COMM,
  TRAD_COMMi = TRAD_COMM,
  REGr = REG,
  REGs = REG,
  stringsAsFactors = FALSE
)
atall_full$Value <- 0
atall_full <- atall_full[do.call(order, atall_full), ]
atall <- atall_full[
  !(atall_full$REGr == "chn" & atall_full$REGs == "usa"),
]
atall$Value <- runif(nrow(atall))

# define custom percentage change shocks
pop_shk   <- ems_custom_shock(var = "pop",    input = pop)
aoall_shk  <- ems_custom_shock(var = "aoall",  input = aoall)
afeall_shk <- ems_custom_shock(var = "afeall", input = afeall)
atall_shk  <- ems_custom_shock(var = "atall",  input = atall)

ems_option_set(write_sub_dir = "custom_partial")

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

# checks
fill_missing <- function(partial, full) {
  merged <- merge(partial, full, by = setdiff(colnames(partial), "Value"), all.y = TRUE)
  merged$Value.x <- ifelse(is.na(merged$Value.x), merged$Value.y, merged$Value.x)
  merged <- merged[, seq_len(ncol(merged) - 1)]
  colnames(merged)[ncol(merged)] <- "Value"
  merged
}

pop    <- fill_missing(pop, pop_full)
aoall  <- fill_missing(aoall, aoall_full)
afeall <- fill_missing(afeall, afeall_full)
atall  <- fill_missing(atall, atall_full)

pop_check    <- all.equal(pop,    outputs$dat$pop, check.attributes = FALSE, tolerance = 1e-6)
aoall_check  <- all.equal(aoall,  outputs$dat$aoall, check.attributes = FALSE, tolerance = 1e-6)
afeall_check <- all.equal(afeall, outputs$dat$afeall, check.attributes = FALSE, tolerance = 1e-6)
atall_check  <- all.equal(atall,  outputs$dat$atall, check.attributes = FALSE, tolerance = 1e-6)

checks <- c(pop_check, aoall_check, afeall_check, atall_check)
