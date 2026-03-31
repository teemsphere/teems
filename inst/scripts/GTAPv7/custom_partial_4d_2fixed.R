# 4D variable (atall: MARGm x COMMc x REGs x REGd), 2 dims fixed
# With MARGm size=1, reduce_shock picks free_idx=[REGd] (k=1)
#   {REGd}(size-1, factor=3) ties {MARGm,REGd}(size-2, factor=3) -> smaller size wins
# write_ragged: single-column branch

.data <- ems_data(
  dat_input = dat_input,
  par_input = par_input,
  set_input = set_input,
  REG = "big3",
  COMM = "macro_sector",
  ACTS = "macro_sector",
  ENDW = "labor_agg",
  target_format = target_format
)

model <- ems_model(
  model_file = model_file,
  closure_file = closure_file
)

REG <- c("chn", "usa", "row")
COMM <- c("svces", "food", "crops", "mnfcs", "livestock")
MARG <- "svces"

set.seed(42)

atall_full <- expand.grid(
  MARGm = MARG,
  COMMc = COMM,
  REGs = REG,
  REGd = REG,
  stringsAsFactors = FALSE
)
atall_full$Value <- 0
atall_full <- atall_full[do.call(order, atall_full), ]

# Fix COMMc="crops" AND REGs="chn"; free dims MARGm(1) x REGd(3), n=3
# reduce_shock: {REGd}(factor=3) ties {MARGm,REGd}(factor=3) -> k=1, free_idx=[REGd]
atall <- atall_full[
  atall_full$COMMc == "crops" & atall_full$REGs == "chn",
]
atall$Value <- runif(nrow(atall))

atall_shk <- ems_custom_shock(var = "atall", input = atall)

ems_option_set(write_sub_dir = "custom_partial_4d_2fixed")

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

check <- all.equal(atall, outputs$dat$atall, check.attributes = FALSE, tolerance = 1e-6)
