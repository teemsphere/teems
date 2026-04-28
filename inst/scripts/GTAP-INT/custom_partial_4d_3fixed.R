# 4D variable (afeall: ENDW_COMMi x PROD_COMMj x REGr x ALLTIMEt), 3 dims fixed
# reduce_shock picks free_idx=[ALLTIMEt] (k=1)
# write_ragged: single-column branch

time_steps <- c(0, 1, 2, 3)

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

model <- ems_model(
  model_file = model_file,
  closure_file = closure_file
)

REG <- c("chn", "usa", "row")
ENDW_COMM <- c("labor", "capital", "natlres", "land")
PROD_COMM <- c("svces", "food", "crops", "mnfcs", "livestock", "cgds")
ALLTIME <- seq(0, length(time_steps) - 1)

set.seed(42)

afeall_full <- expand.grid(
  ENDW_COMMi = ENDW_COMM,
  PROD_COMMj = PROD_COMM,
  REGr = REG,
  ALLTIMEt = ALLTIME,
  stringsAsFactors = FALSE
)
afeall_full$Value <- 0
afeall_full <- afeall_full[do.call(order, afeall_full), ]

# Fix ENDW_COMMi="natlres", PROD_COMMj="crops", REGr="chn"; free dim ALLTIMEt(4), n=4
# reduce_shock: all 2-element free subsets need >=9 rows; only [ALLTIMEt](4) matches -> k=1
afeall <- afeall_full[
  afeall_full$ENDW_COMMi == "natlres" &
  afeall_full$PROD_COMMj == "crops" &
  afeall_full$REGr == "chn",
]
afeall$Value <- runif(nrow(afeall))

afeall_shk <- ems_custom_shock(var = "afeall", input = afeall)

ems_option_set(write_sub_dir = "custom_partial_4d_3fixed")

cmf_path <- ems_deploy(
  write_dir = write_dir,
  .data = .data,
  model = model,
  shock = afeall_shk
)

outputs <- ems_solve(
  cmf_path = cmf_path,
  matrix_method = "LU",
  solution_method = "Johansen"
)

# checks
afeall <- merge(afeall, afeall_full, by = setdiff(colnames(afeall), "Value"), all.y = TRUE)
afeall$Value.x <- ifelse(is.na(afeall$Value.x), afeall$Value.y, afeall$Value.x)
afeall <- afeall[, seq_len(ncol(afeall) - 1)]
colnames(afeall)[ncol(afeall)] <- "Value"

check <- all.equal(afeall, outputs$dat$afeall[, !"Year"], check.attributes = FALSE, tolerance = 1e-6)
