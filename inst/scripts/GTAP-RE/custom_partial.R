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

set.seed(42)

# 2D: remove 2 specific (REG, ALLTIME) elements — irregular, exercises element-by-element fallback
pop_full <- expand.grid(
  REGr = REG,
  ALLTIMEt = ALLTIME,
  stringsAsFactors = FALSE
)
pop_full$Value <- 0
pop_full <- pop_full[do.call(order, pop_full), ]
pop <- pop_full[!(pop_full$REGr == "chn" & pop_full$ALLTIMEt %in% c(2, 3)), ]
pop$Value <- runif(nrow(pop))

# 3D: remove rows that break complete-group formation for every free_idx subset
aoall_full <- expand.grid(
  ACTSa = ACTS,
  REGr = REG,
  ALLTIMEt = ALLTIME,
  stringsAsFactors = FALSE
)
aoall_full$Value <- 0
aoall_full <- aoall_full[do.call(order, aoall_full), ]
aoall <- aoall_full[
  !(aoall_full$ACTSa == "crops" & aoall_full$REGr == "usa") &
  aoall_full$ALLTIMEt != 3,
]
aoall$Value <- runif(nrow(aoall))

# 4D: remove by two simultaneous dim conditions — breaks all potential complete groups
afeall_full <- expand.grid(
  ENDWe = ENDW,
  ACTSa = ACTS,
  REGr = REG,
  ALLTIMEt = ALLTIME,
  stringsAsFactors = FALSE
)
afeall_full$Value <- 0
afeall_full <- afeall_full[do.call(order, afeall_full), ]
afeall <- afeall_full[
  afeall_full$ENDWe != "natlres" & afeall_full$REGr != "row",
]
afeall$Value <- runif(nrow(afeall))

# 5D: deterministic removal — remove all (REGs=="chn", ALLTIMEt %in% c(2,3)) tuples
atall_full <- expand.grid(
  MARGm = MARG,
  COMMc = COMM,
  REGs = REG,
  REGd = REG,
  ALLTIMEt = ALLTIME,
  stringsAsFactors = FALSE
)
atall_full$Value <- 0
atall_full <- atall_full[do.call(order, atall_full), ]
atall <- atall_full[
  !(atall_full$REGs == "chn" & atall_full$ALLTIMEt %in% c(2, 3)),
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

pop    <- fill_missing(pop,    pop_full)
aoall  <- fill_missing(aoall,  aoall_full)
afeall <- fill_missing(afeall, afeall_full)
atall  <- fill_missing(atall,  atall_full)

pop_check    <- all.equal(pop,    outputs$dat$pop[, !"Year"],    check.attributes = FALSE, tolerance = 1e-6)
aoall_check  <- all.equal(aoall,  outputs$dat$aoall[, !"Year"],  check.attributes = FALSE, tolerance = 1e-6)
afeall_check <- all.equal(afeall, outputs$dat$afeall[, !"Year"], check.attributes = FALSE, tolerance = 1e-6)
atall_check  <- all.equal(atall,  outputs$dat$atall[, !"Year"],  check.attributes = FALSE, tolerance = 1e-6)

checks <- c(pop_check, aoall_check, afeall_check, atall_check)
