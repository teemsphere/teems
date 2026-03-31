# 2D variable (pop: REGr x ALLTIMEt), 1 dim fixed
# reduce_shock picks free_idx=[ALLTIMEt] (k=1)
# write_ragged: single-column branch

time_steps <- c(0, 1, 2, 3)

.data <- ems_data(
  dat_input = dat_input,
  par_input = par_input,
  set_input = set_input,
  REG = "big3",
  COMM = "macro_sector",
  ACTS = "macro_sector",
  ENDW = "labor_agg",
  time_steps = time_steps,
  target_format = target_format
)

model <- ems_model(
  model_file = model_file,
  closure_file = closure_file
)

REG <- c("chn", "usa", "row")
ALLTIME <- seq(0, length(time_steps) - 1)

set.seed(42)

pop_full <- expand.grid(
  REGr = REG,
  ALLTIMEt = ALLTIME,
  stringsAsFactors = FALSE
)
pop_full$Value <- 0
pop_full <- pop_full[do.call(order, pop_full), ]

# Fix REGr to one value; free dim is ALLTIMEt (size 4)
# reduce_shock: free_idx=[ALLTIMEt], n_free=4, het_grp -> k=1
pop <- pop_full[pop_full$REGr == "chn", ]
pop$Value <- runif(nrow(pop))

pop_shk <- ems_custom_shock(var = "pop", input = pop)

ems_option_set(write_sub_dir = "custom_partial_2d_1fixed")

cmf_path <- ems_deploy(
  write_dir = write_dir,
  .data = .data,
  model = model,
  shock = pop_shk
)

outputs <- ems_solve(
  cmf_path = cmf_path,
  matrix_method = "LU",
  solution_method = "Johansen"
)

# checks
pop <- merge(pop, pop_full, by = setdiff(colnames(pop), "Value"), all.y = TRUE)
pop$Value.x <- ifelse(is.na(pop$Value.x), pop$Value.y, pop$Value.x)
pop <- pop[, seq_len(ncol(pop) - 1)]
colnames(pop)[ncol(pop)] <- "Value"

check <- all.equal(pop, outputs$dat$pop[, !"Year"], check.attributes = FALSE, tolerance = 1e-6)
