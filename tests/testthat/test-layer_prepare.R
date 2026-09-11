skip_on_cran()

ems_option_set(verbose = FALSE)
withr::defer(ems_option_reset(), teardown_env())

fmt <- "GTAPv7"
mk_set <- function(h, ele) structure(ele, class = c(h, h, "set", fmt, "character"))
mk_arr <- function(h, kind, dims, value = 1) {
  a <- array(value, dim = lengths(dims), dimnames = dims)
  class(a) <- c(h, kind, fmt, class(a))
  a
}
# BLOC and REGTOBLOC are read from the parameter file, so the loader
# classes them as parameters
mk_bloc <- function(h, ele) structure(ele, class = c(h, "par", fmt, "character"))
reg <- c("chn", "row")
comm <- c("pdr", "coa", "oil", "gas", "p_c", "ely", "gdt", "mnfcs")
come <- c("coa", "oil", "gas", "p_c", "ely", "gdt")
fuel <- c("coa", "oil", "gas", "p_c", "gdt")
topp <- c("eny", "pdr", "mnfcs")

synthetic_e <- function() {
  i_data <- list(
    REG = mk_set("REG", reg),
    COMM = mk_set("COMM", comm),
    ACTS = mk_set("ACTS", comm),
    FUEL = mk_set("FUEL", fuel),
    COME = mk_set("COME", come),
    SUBP = mk_arr("SUBP", "par", list(COMM = comm, REG = reg), 0.5),
    INCP = mk_arr("INCP", "par", list(COMM = comm, REG = reg), 1.2),
    SUBE = mk_arr("SUBE", "par", list(TOPP = topp, REG = reg), 0.6),
    INCE = mk_arr("INCE", "par", list(TOPP = topp, REG = reg), 1.4),
    TRBL = mk_bloc("TRBL", reg),
    MAPB = mk_bloc("MAPB", reg),
    VDFB = mk_arr("VDFB", "dat", list(COMM = comm, ACTS = comm, REG = reg))
  )
  attr(i_data, "metadata") <- list(data_format = fmt, database_version = "GTAPv12")
  class(i_data) <- c(fmt, "list")
  i_data
}

test_that("GTAP-E preparation on a synthetic layer", {
  i_data <- synthetic_e()
  expect_true(.is_e_input(i_data))
  expect_false(.is_ep_input(i_data))
  out <- .prepare_e(i_data, call = NULL)

  # the flag is exact: `$e` on a list would partial-match `ep`
  expect_true(isTRUE(attr(out, "metadata")[["e"]]))
  expect_null(attr(out, "metadata")[["ep"]])
  expect_false(.is_e_input(out))
  expect_identical(class(out), c(fmt, "list"))

  # the disaggregated lists at source, the energy sets over COME, TOPP
  # as the energy node plus the non-energy commodities
  expect_equal(as.character(out$DCOM), comm)
  expect_equal(as.character(out$MCOM), comm)
  expect_equal(as.character(out$DELY), "ely")
  for (h in c("EGY", "ENYP", "ENYG", "ENYI")) {
    expect_equal(as.character(out[[h]]), come)
    expect_null(attr(out[[h]], "user_set"))
  }
  expect_equal(as.character(out$TOPP), topp)
  for (h in c("DCOM", "MCOM", "DELY")) {
    expect_true(isTRUE(attr(out[[h]], "user_set")))
  }

  # SUBE/INCE bound to the names the model reads, the COMM-dimensioned
  # incumbents dropped, the bloc headers reclassed as sets
  expect_equal(sum(names(out) == "SUBP"), 1L)
  expect_equal(sum(names(out) == "INCP"), 1L)
  expect_false(any(c("SUBE", "INCE") %in% names(out)))
  expect_equal(names(dimnames(out$SUBP)), c("TOPP", "REG"))
  expect_equal(class(out$SUBP)[1:2], c("SUBP", "par"))
  expect_equal(unique(as.vector(out$INCP)), 1.4)
  expect_true(inherits(out$TRBL, "set"))
  expect_null(attr(out$TRBL, "user_set"))
  expect_true(inherits(out$MAPB, "set"))
  expect_true(isTRUE(attr(out$MAPB, "user_set")))

  # the new sets follow the leading set block; everything else keeps
  # its place
  expect_equal(names(out), c(
    "REG", "COMM", "ACTS", "FUEL", "COME",
    "DCOM", "MCOM", "DELY", "EGY", "ENYP", "ENYG", "ENYI", "TOPP",
    "SUBP", "INCP", "TRBL", "MAPB", "VDFB"
  ))

  # named aborts: an incomplete layer, a mis-dimensioned CDE parameter,
  # the un-normalised 11c row
  expect_snapshot_error(.prepare_e(i_data[names(i_data) != "FUEL"], call = NULL))
  bad_dim <- i_data
  bad_dim$SUBE <- mk_arr("SUBE", "par", list(COMM = comm, REG = reg), 0.6)
  expect_snapshot_error(.prepare_e(bad_dim, call = NULL))
  bad_row <- i_data
  bad_row$SUBE["eny", ] <- 5.078
  expect_snapshot_error(.prepare_e(bad_row, call = NULL))
})

test_that("GTAP-EP preparation on a synthetic layer", {
  techs <- c("coalbl", "gasbl", "gasp", "hydrobl", "hydrop", "nuclearbl", "oilbl", "oilp", "otherbl", "solarp", "windbl")
  elec <- c("tnd", techs)
  p_comm <- c("pdr", fuel, elec, "mnfcs")
  # at source COME excludes T&D
  p_come <- c(fuel, techs)
  p_topp <- c("eny", "pdr", "mnfcs")
  i_data <- list(
    REG = mk_set("REG", reg),
    COMM = mk_set("COMM", p_comm),
    ACTS = mk_set("ACTS", p_comm),
    FUEL = mk_set("FUEL", fuel),
    COME = mk_set("COME", p_come),
    POWR = mk_set("POWR", elec),
    ELEA = mk_set("ELEA", techs),
    ELEC = mk_set("ELEC", elec),
    SUBP = mk_arr("SUBP", "par", list(COMM = p_comm, REG = reg), 0.5),
    INCP = mk_arr("INCP", "par", list(COMM = p_comm, REG = reg), 1.2),
    SUBE = mk_arr("SUBE", "par", list(TOPP = p_topp, REG = reg), 0.6),
    INCE = mk_arr("INCE", "par", list(TOPP = p_topp, REG = reg), 1.4),
    VDFB = mk_arr("VDFB", "dat", list(COMM = p_comm, ACTS = p_comm, REG = reg))
  )
  attr(i_data, "metadata") <- list(data_format = fmt, database_version = "GTAPv12")
  class(i_data) <- c(fmt, "list")
  expect_true(.is_ep_input(i_data))
  expect_false(.is_e_input(i_data))
  out <- .prepare_ep(i_data, call = NULL)
  expect_true(isTRUE(attr(out, "metadata")[["ep"]]))
  expect_null(attr(out, "metadata")[["e"]])
  expect_false(.is_ep_input(out))

  expect_equal(as.character(out$DCOM), p_comm)
  expect_equal(as.character(out$DELY), elec)
  # EGY is the fuels plus the whole electricity list (T&D included),
  # the ENY* sets the fuels plus the synthetic nest node
  expect_equal(as.character(out$EGY), c(fuel, elec))
  expect_equal(as.character(out$TOPP), p_topp)
  for (agent in c("P", "G", "I")) {
    expect_equal(as.character(out[[paste0("ENY", agent)]]), c(fuel, "ely"))
  }
  expect_false("ENYF" %in% names(out))
  base <- c("coalbl", "gasbl", "hydrobl", "nuclearbl", "oilbl", "otherbl", "windbl")
  peak <- c("gasp", "hydrop", "oilp", "solarp")
  for (agent in c("F", "G", "I", "P")) {
    expect_equal(as.character(out[[paste0("ELE", agent)]]), c("ely", "egen", "ebl", "epl"))
    expect_equal(as.character(out[[paste0("ELY", agent)]]), c("egen", "tnd"))
    expect_equal(as.character(out[[paste0("EGN", agent)]]), c("ebl", "epl"))
    expect_equal(as.character(out[[paste0("EBL", agent)]]), base)
    expect_equal(as.character(out[[paste0("EPL", agent)]]), peak)
    expect_null(attr(out[[paste0("EBL", agent)]], "user_set"))
    expect_true(isTRUE(attr(out[[paste0("ELE", agent)]], "user_set")))
  }
  expect_equal(names(dimnames(out$SUBP)), c("TOPP", "REG"))
  expect_false(any(c("SUBE", "INCE") %in% names(out)))
  expect_equal(which(names(out) == "DCOM"), 9L)
  expect_true(inherits(out$VDFB, "dat"))

  # a technology list the BL/P suffixes do not partition is named
  expect_equal(.ep_load_split(techs), list(base = base, peak = peak))
  expect_null(.ep_load_split(c("coalbl", "gasp", "nuclear")))
  bad_split <- i_data
  bad_split$ELEA <- mk_set("ELEA", c("coalbl", "gasp", "nuclear"))
  expect_snapshot_error(.prepare_ep(bad_split, call = NULL))
  expect_snapshot_error(.prepare_ep(i_data[names(i_data) != "ELEA"], call = NULL))
})

test_that("mapping buckets follow the exact layer flags", {
  expect_equal(.map_layers(list(aez = TRUE, ep = TRUE)), c("GTAP-AEZ", "GTAP-EP"))
  expect_equal(.map_layers(list(ep = TRUE)), "GTAP-EP")
  # GTAP-E redefines no element list and contributes no bucket; the
  # flag must not partial-match the Power one
  expect_null(.map_layers(list(e = TRUE)))
  expect_null(.map_layers(list()))
})

test_that("a COMM weight is recast onto TOPP", {
  w <- data.table::data.table(
    COMM = rep(comm, each = 2), REG = rep(reg, length(comm)), Value = 1
  )
  out <- .weight_over_topp(w, topp = topp, e_header = "VDPB")
  expect_equal(names(out), c("TOPP", "REG", "Value"))
  expect_setequal(unique(out$TOPP), topp)
  expect_equal(out[TOPP == "eny", .N], length(come) * length(reg))
  # the input is untouched: the Armington parameters still read it over COMM
  expect_equal(names(w), c("COMM", "REG", "Value"))
  # an array weight is accepted as well
  arr <- mk_arr("VDPB", "dat", list(COMM = comm, REG = reg))
  expect_equal(names(.weight_over_topp(arr, topp = topp, e_header = "VDPB")), c("TOPP", "REG", "Value"))
  # named aborts: no weight, no COMM dimension, no single energy node
  expect_snapshot_error(.weight_over_topp(NULL, topp = topp, e_header = "VDPB"))
  expect_snapshot_error(.weight_over_topp(w[, .(REG, Value)], topp = topp, e_header = "VDPB"))
  expect_snapshot_error(.weight_over_topp(w, topp = c("eny", "nrg", "pdr", "mnfcs"), e_header = "VDPB"))
})

test_that("the IF rewrite's element-equality set is read off its declaration", {
  model <- data.frame(
    type = c("Set", "Set", "Coefficient"),
    tab = c(
      "Set IFS1 # coal narrowing # = \"coa\" & DCOMM;",
      "Set IFS2 # remainder # = DCOMM - IFS1;",
      "Coefficient (all,c,DCOMM) UNITDCOAL(c);"
    ),
    stringsAsFactors = FALSE
  )
  expect_equal(.if_rewrite_elements(model, "IFS1"), "coa")
  expect_equal(.if_rewrite_elements(model, "ifs1"), "coa")
  expect_null(.if_rewrite_elements(model, "IFS2"))
  expect_null(.if_rewrite_elements(model, "IFS3"))
  expect_null(.if_rewrite_elements(NULL, "IFS1"))
})
