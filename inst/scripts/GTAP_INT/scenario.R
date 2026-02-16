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

pop <- ems_data(
  dat_input = dat_input,
  par_input = par_input,
  set_input = set_input,
  REG = "full",
  TRAD_COMM = "macro_sector",
  ENDW_COMM = "labor_agg",
  target_format = target_format
)$pop

pop$Year <- year
regions <- unique(pop$REG)
pop_traj <- expand.grid(
  REG = regions,
  Value = 0,
  Year = tail(time_steps, -1),
  stringsAsFactors = FALSE
)

pop <- rbind(pop, pop_traj)

growth_rates <- data.frame(
  REG = regions,
  growth_rate = runif(length(regions), min = -0.01, max = 0.05)
)

pop <- merge(pop, growth_rates, by = "REG")
base_values <- pop[pop$Year == year, c("REG", "Value")]
names(base_values)[2] <- "base_value"
pop <- merge(pop, base_values, by = "REG")

pop$Value[pop$Year > year] <-
  pop$base_value[pop$Year > year] *
    (1 + pop$growth_rate[pop$Year > year])^(pop$Year[pop$Year > year] - year)
pop$growth_rate <- NULL
pop$base_value <- NULL
pop <- pop[order(pop$REG, pop$Year), ]
pop <- pop[, c("REG", "Year", "Value")]
colnames(pop)[1] <- "REGr"

pop_trajectory <- ems_shock(
  var = "pop",
  type = "scenario",
  input = pop
)

ems_option_set(write_sub_dir = "scenario")

cmf_path <- ems_deploy(
  write_dir = write_dir,
  .data = .data,
  model = model,
  shock = pop_trajectory
)

outputs <- ems_solve(
  cmf_path = cmf_path,
  matrix_method = "LU",
  solution_method = "Johansen"
)