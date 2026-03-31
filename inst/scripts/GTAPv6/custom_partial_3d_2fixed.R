# 3D variable (afeall: ENDW_COMMi x PROD_COMMj x REGr), 2 dims fixed
# reduce_shock picks free_idx=[REGr] (k=1)
# write_ragged: single-column branch

.data <- ems_data(
  dat_input = dat_input,
  par_input = par_input,
  set_input = set_input,
  REG = "big3",
  TRAD_COMM = "macro_sector",
  ENDW_COMM = "labor_agg",
  target_format = target_format
)

model <- ems_model(
  model_file = model_file,
  closure_file = closure_file
)

REG <- c("chn", "usa", "row")
ENDW_COMM <- c("labor", "capital", "natlres", "land")
PROD_COMM <- c("svces", "food", "crops", "mnfcs", "livestock", "zcgds")

set.seed(42)

afeall_full <- expand.grid(
  ENDW_COMMi = ENDW_COMM,
  PROD_COMMj = PROD_COMM,
  REGr = REG,
  stringsAsFactors = FALSE
)
afeall_full$Value <- 0
afeall_full <- afeall_full[do.call(order, afeall_full), ]

# Fix ENDW_COMMi="labor" AND PROD_COMMj="crops"; free dim is REGr(3), n=3
# reduce_shock: free_idx=[REGr], k=1
afeall <- afeall_full[
  afeall_full$ENDW_COMMi == "labor" & afeall_full$PROD_COMMj == "crops",
]
afeall$Value <- runif(nrow(afeall))

afeall_shk <- ems_custom_shock(var = "afeall", input = afeall)

ems_option_set(write_sub_dir = "custom_partial_3d_2fixed")

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

check <- all.equal(afeall, outputs$dat$afeall, check.attributes = FALSE, tolerance = 1e-6)
