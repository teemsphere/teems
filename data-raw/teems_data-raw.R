library(targets)

targets::tar_config_set(store = "./data-raw/_targets")

# Set target options:
targets::tar_option_set(
  packages = c("data.table", "usethis", "purrr", "tabulapdf"),
  format = "qs",
  cue = tar_cue("always")
)



# functions
targets::tar_source("./data-raw/R")

list(
  tar_target(db_version, c("GTAPv9", "GTAPv10", "GTAPv11")),
  tar_target(data_format, c("GTAPv6", "GTAPv7")),
  # mapping related --------------------------------------------------
  tar_target(mapping_files, {
    list.files(
      "../teems-mappings",
      pattern = "\\.csv",
      recursive = TRUE,
      full.names = TRUE
    )
  }),
  tar_target(
    mappings,
    process_mappings(
      mapping_files = mapping_files,
      db_version = db_version,
      data_format = data_format
    )
  ),
  tar_target(tab_qual, {
    c(
      "nonzero_by_zero",
      "zero_by_zero",
      "\\bchange\\b",
      "linear",
      "orig_level",
      "ge [[:digit:]]",
      "gt [[:digit:]]",
      "integer",
      "parameter",
      "initial",
      "\\breal\\b",
      "levels"
    )
  }),

  # parameter related ------------------------------------------------
  # static ===========================================================
  tar_target(GTAPv6_weights, {
    list(
      ESBD = c("VDPA", "VIPA", "VDGA", "VIGA", "VDFA", "VIFA"),
      ESBM = c("VIPA", "VIGA", "VIFA"),
      ESBT = c("VDFM", "VIFM", "VFM", "FTRV", "-FBEP", "-ISEP"),
      ESBV = "EVFA",
      INCP = c("VDPA", "VIPA"),
      SUBP = c("VDPA", "VIPA")
    )
  }),
  tar_target(GTAPv7_weights, {
    list(
      ESBD = c("VDPP", "VMPP", "VMGP", "VDGP", "VDFP", "VMFP"),
      ESBM = c("VMPP", "VMGP", "VMFP"),
      ESBT = c("VDFB", "VMFB", "EVFB", "FTRV", "-FBEP", "-ISEP"),
      ESBV = "EVFP",
      INCP = c("VDPP", "VMPP"),
      SUBP = c("VDPP", "VMPP"),
      ESBC = c("VDFB", "VMFB")
    )
  }),
  tar_target(param_weights, {
    list(GTAPv6 = GTAPv6_weights, GTAPv7 = GTAPv7_weights)
  }),
  # v6 <> v7 conversion
  tar_target(
    GTAPv7_manual,
    "./data-raw/Corong and Tsigas - 2017 - The Standard GTAP Model, Version 7.pdf",
    format = "file"
  ),
  # tables here are not even concordance, just semi related lists
  tar_target(set_conversion, {
    set_table <- tabulapdf::extract_tables(file = GTAPv7_manual, pages = 83)
    c_names <- c("name", "header", "description")

    GTAPv6_sets <- set_table[[1]][, c(1:4)]
    colnames(x = GTAPv6_sets) <- c("idx", paste0("GTAPv6", c_names))
    GTAPv7_sets <- set_table[[1]][, c(5:8)]
    colnames(x = GTAPv7_sets) <- c("idx", paste0("GTAPv7", c_names))

    # always inconsistencies in GTAP outputs
    GTAPv7_sets[5:13, "idx"] <- 5:13
    GTAPv7_sets[12:13, "idx"] <- 13:14

    sets <- merge(GTAPv6_sets, GTAPv7_sets, by = "idx", all = TRUE)
  }),
  tar_target(param_conversion, {
    param_table <- tabulapdf::extract_tables(file = GTAPv7_manual, pages = 84)

    GTAPv7_param <- param_table[[1]][, c(5:8)]

    # missing ESBQ
    ESBQ <- tibble::tibble(
      14,
      "ESBQ",
      "COMM*REG",
      "1/CES elasticity for commodity sourcing"
    )
    colnames(x = ESBQ) <- colnames(x = GTAPv7_param)

    # missing ESBI
    # ESBI <- tibble::tibble(15, "ESBI", "REG", "Investment expenditure CES elasticity")
    # colnames(x = ESBI) <- colnames(x = v7_param)

    GTAPv7_param <- rbind(GTAPv7_param, ESBQ)

    GTAPv7_param <- .table_fix(
      single = c(11, 12, 27),
      double = c(1, 3, 5, 7, 9, 13, 15, 17, 19, 25),
      trebble = c(19, 22),
      table = GTAPv7_param,
      prefix = "GTAPv7",
      data_type = "par"
    )

    GTAPv7_param[10:14, "idx"] <- 11:15

    GTAPv6_param <- param_table[[1]][, c(1:4)]

    GTAPv6_param <- .table_fix(
      single = c(11, 12),
      double = c(1, 3, 5, 7, 9, 13, 15, 17),
      table = GTAPv6_param,
      prefix = "GTAPv6",
      data_type = "par"
    )

    param <- merge(GTAPv6_param, GTAPv7_param, by = "idx", all = TRUE)
    param[["data_type"]] <- "par"
    return(param)
  }),
  tar_target(dat_conversion, {
    coeff_table <- tabulapdf::extract_tables(file = GTAPv7_manual, pages = 85:86)

    coeff_table <- data.table::rbindlist(l = coeff_table)

    GTAPv7_coeff <- coeff_table[, c(4:6)]
    double <- c(3, 18, 20, 22, 24, 26, 30, 32, 35, 37, 41, 49, 51, 54)
    single <- setdiff(seq(1, nrow(GTAPv7_coeff)), double)
    NAs <- which(is.na(GTAPv7_coeff[, 1]))
    single <- setdiff(single, NAs)
    GTAPv7_coeff <- .table_fix(
      single = single,
      double = double,
      table = GTAPv7_coeff,
      prefix = "GTAPv7",
      data_type = "dat"
    )

    GTAPv6_coeff <- coeff_table[, c(1:3)]
    double <- c(18, 20, 22, 24, 26, 30, 32, 35, 37, 41)
    single <- setdiff(seq(1, nrow(GTAPv6_coeff)), double)

    NAs <- which(is.na(GTAPv6_coeff[, 1]))
    single <- setdiff(single, NAs)
    GTAPv6_coeff <- .table_fix(
      single = single,
      double = double,
      table = GTAPv6_coeff,
      prefix = "GTAPv6",
      data_type = "dat"
    )

    coeff <- merge(GTAPv6_coeff, GTAPv7_coeff, by = "idx", all = TRUE)
    coeff[["GTAPv6set"]] <- NA
    coeff[["GTAPv7set"]] <- NA
    coeff[["data_type"]] <- "dat"
    return(coeff)
  }),
  tar_target(coeff_conversion, {
    rbind(dat_conversion, param_conversion)
  }),
  # messages (definitions in data-raw/R/msg_*.R) -------------------------
  tar_target(gen_err, build_gen_err()),
  tar_target(gen_wrn, build_gen_wrn()),
  tar_target(gen_info, build_gen_info()),
  tar_target(gen_url, build_gen_url()),
  tar_target(data_err, build_data_err()),
  tar_target(data_wrn, build_data_wrn()),
  tar_target(load_err, build_load_err()),
  tar_target(model_wrn, build_model_wrn()),
  tar_target(model_err, build_model_err()),
  tar_target(deploy_err, build_deploy_err()),
  tar_target(set_err, build_set_err()),
  tar_target(set_wrn, build_set_wrn()),
  tar_target(cls_err, build_cls_err()),
  tar_target(shk_err, build_shk_err()),
  tar_target(shk_infm, build_shk_infm()),
  tar_target(shk_url, build_shk_url()),
  tar_target(swap_err, build_swap_err()),
  tar_target(solve_err, build_solve_err()),
  tar_target(solve_wrn, build_solve_wrn()),
  tar_target(solve_info, build_solve_info()),
  tar_target(compose_err, build_compose_err()),
  tar_target(aux_err, build_aux_err()),
  tar_target(aux_wrn, build_aux_wrn()),
  # internal data ====================================================
  tar_target(internal_data, {
    usethis::use_data(
      mappings,
      tab_qual,
      param_weights,
      set_conversion,
      coeff_conversion,
      aux_err,
      gen_info,
      gen_wrn,
      gen_err,
      gen_url,
      data_wrn,
      data_err,
      model_wrn,
      model_err,
      load_err,
      deploy_err,
      shk_err,
      swap_err,
      set_err,
      cls_err,
      shk_infm,
      shk_url,
      solve_err,
      solve_wrn,
      solve_info,
      compose_err,
      aux_err,
      aux_wrn,
      # check_wrn,
      # check_err,
      overwrite = TRUE,
      internal = TRUE
    )
  })
)