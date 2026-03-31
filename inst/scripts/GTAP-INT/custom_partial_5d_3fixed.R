# 5D variable (atall: MARG_COMMm x TRAD_COMMi x REGr x REGs x ALLTIMEt), 3 dims fixed
#
# reduce_shock picks free_idx=[ALLTIMEt] (k=1)
#   [5] (size-1, factor=4) is tried before [1,5] (size-2, same factor=4)
#   because size-1 subsets precede size-2 in the original all_free list
# write_ragged: single-column branch

time_steps <- c(0, 1, 2, 3)

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
  model_file = model_file,
  closure_file = closure_file
)

REG <- c("chn", "usa", "row")
TRAD_COMM <- c("svces", "food", "crops", "mnfcs", "livestock")
MARG_COMM <- "svces"
ALLTIME <- seq(0, length(time_steps) - 1)

set.seed(42)

atall_full <- expand.grid(
  MARG_COMMm = MARG_COMM,
  TRAD_COMMi = TRAD_COMM,
  REGr = REG,
  REGs = REG,
  ALLTIMEt = ALLTIME,
  stringsAsFactors = FALSE
)
atall_full$Value <- 0
atall_full <- atall_full[do.call(order, atall_full), ]

# Fix TRAD_COMMi="crops", REGr="chn", REGs="usa"; free dim ALLTIMEt(4), n=4
# reduce_shock: [5] (factor=4, size-1) is tried before [1,5] (factor=4, size-2) -> k=1
atall <- atall_full[
  atall_full$TRAD_COMMi == "crops" &
  atall_full$REGr == "chn" &
  atall_full$REGs == "usa",
]
atall$Value <- runif(nrow(atall))

atall_shk <- ems_custom_shock(var = "atall", input = atall)

ems_option_set(write_sub_dir = "custom_partial_5d_3fixed")

cmf_path <- ems_deploy(
  write_dir = write_dir,
  .data = .data,
  model = model,
  shock = atall_shk
)

outputs <- ems_solve(
  cmf_path = cmf_path,
  matrix_method = "LU",
  solution_method = "Johansen"
)

# checks
atall <- merge(atall, atall_full, by = setdiff(colnames(atall), "Value"), all.y = TRUE)
atall$Value.x <- ifelse(is.na(atall$Value.x), atall$Value.y, atall$Value.x)
atall <- atall[, seq_len(ncol(atall) - 1)]
colnames(atall)[ncol(atall)] <- "Value"

check <- all.equal(atall, outputs$dat$atall[, !"Year"], check.attributes = FALSE, tolerance = 1e-6)
