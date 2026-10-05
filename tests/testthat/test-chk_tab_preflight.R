skip_on_cran()

write_dir <- file.path(tools::R_user_dir("teems", "cache"), "preflight_test")
if (dir.exists(write_dir)) {
  unlink(write_dir, recursive = TRUE)
}
dir.create(write_dir, recursive = TRUE)
ems_option_set(verbose = FALSE, tempdir = write_dir)
withr::defer(ems_option_reset(), teardown_env())

model_files <- ems_example("GTAPv7", write_dir)
model_file <- model_files[["model_file"]]
base_txt <- readChar(model_file, file.info(model_file)$size)

mutate_tab <- function(text, name = "mut.tab") {
  path <- file.path(write_dir, name)
  writeChar(paste0(base_txt, "\n", text, "\n"), path, eos = NULL)
  path
}

expect_preflight_error <- function(text) {
  expect_snapshot_error(
    quiet_pivot(.process_tablo(tab_file = mutate_tab(text), quiet = TRUE, call = NULL))
  )
}

# names (GEMPACK manual 11.2.1; solver names_validate)

test_that("name collisions abort", {
  expect_preflight_error("Coefficient (all,r,REG) PSAVE(r);")
  expect_preflight_error("Coefficient REG;")
  expect_preflight_error("Variable (all,r,REG) reg(r);")
})

test_that("duplicate declarations abort", {
  expect_preflight_error("Coefficient DUPX;\nCoefficient DUPX;")
})

test_that("reserved words abort", {
  expect_preflight_error("Coefficient (all,r,REG) MAX(r);")
})

test_that("coefficients named for a levels variable's linear variable abort", {
  expect_preflight_error(paste(
    "Variable (levels,change) LCH;",
    "Formula (initial) LCH = 1;",
    "Coefficient c_LCH;",
    sep = "\n"
  ))
  expect_preflight_error(paste(
    "Variable (levels) LPC;",
    "Formula (initial) LPC = 1;",
    "Coefficient p_LPC;",
    sep = "\n"
  ))
})

test_that("p_X and c_X coefficients beside a linear variable X parse", {
  # qgdp is a declared GTAPv7 linear variable (GEMPACK manual 9.2.2)
  quiet_pivot(model <- .process_tablo(
    tab_file = mutate_tab("Coefficient c_qgdp;\nCoefficient p_qgdp;"),
    quiet = TRUE,
    call = NULL
  ))
  expect_true(all(c("c_qgdp", "p_qgdp") %in% model$name))
})

test_that("c_ prefixed coefficients without a variable of the tail name parse", {
  quiet_pivot(model <- .process_tablo(
    tab_file = mutate_tab("Coefficient (all,r,REG) C_EMIS_HAr(r);"),
    quiet = TRUE,
    call = NULL
  ))
  expect_true("C_EMIS_HAr" %in% model$name)
})

test_that("variables named for a levels variable's linear variable abort", {
  expect_preflight_error(paste(
    "Variable (levels) LPV;",
    "Formula (initial) LPV = 1;",
    "Variable p_LPV;",
    sep = "\n"
  ))
})

test_that("p_X variables beside a linear or change levels X parse", {
  quiet_pivot(model <- .process_tablo(
    tab_file = mutate_tab(paste(
      "Variable (all,r,REG) p_qgdp(r);",
      "Variable (levels,change) LCV;",
      "Formula (initial) LCV = 1;",
      "Variable p_LCV;",
      sep = "\n"
    )),
    quiet = TRUE,
    call = NULL
  ))
  expect_true(all(c("p_qgdp", "p_LCV") %in% model$name))
})

test_that("the hand-linearized pair idiom parses", {
  # VKB is a declared GTAPv7 coefficient: coefficient X + variable
  # p_X is the supported pair (solver section-6 naming resolution)
  quiet_pivot(model <- .process_tablo(
    tab_file = mutate_tab(
      "Variable (all,r,REG) p_VKB(r) # pair of coefficient VKB #;"
    ),
    quiet = TRUE,
    call = NULL
  ))
  expect_true("p_VKB" %in% model$name[model$type == "Variable"])
})

test_that("over-length names abort", {
  expect_preflight_error(
    paste0("Coefficient ", strrep("A", 260), ";")
  )
})

# qualifiers (GEMPACK manual 10.3/10.4; solver tab_qualifiers_parse)

test_that("unknown qualifiers abort", {
  expect_preflight_error("Variable (foo) dummyvar;")
})

test_that("no_split qualifier aborts", {
  expect_preflight_error("Variable (no_split) dummyvar;")
})

test_that("linear_name and linear_var qualifiers parse", {
  quiet_pivot(model <- .process_tablo(
    tab_file = mutate_tab(paste(
      "Variable (levels, linear_name=xlin) LNM;",
      "Formula (initial) LNM = 1;",
      "Variable (all,r,REG) xlv(r);",
      "Variable (levels, linear_var=xlv) (all,r,REG) LVR(r);",
      "Formula (initial) (all,r,REG) LVR(r) = 1;",
      sep = "\n"
    )),
    quiet = TRUE,
    call = NULL
  ))
  expect_true(all(c("LNM", "LVR") %in% model$name))
})

test_that("empty qualifiers abort", {
  expect_preflight_error("Variable () dummyvar;")
})

test_that("duplicate bounds abort", {
  expect_preflight_error(
    "Coefficient (ge 0, ge 1) (all,r,REG) BNDP(r);"
  )
})

# Default statements (GEMPACK manual 10.19; solver tab_defaults_validate)

test_that("invalid Default statements abort", {
  expect_preflight_error("Coefficient (default=lower_bound ge 0);")
  expect_preflight_error("Variable (default=foo);")
  expect_preflight_error("Update (default=always);")
  expect_preflight_error("Equation (default=add_homotopy=);")
})

test_that("Default statements apply positionally to the declarations that follow", {
  quiet_pivot(model <- .process_tablo(
    tab_file = mutate_tab(paste(
      "Variable (default=levels);",
      "Variable (default=change);",
      "Equation (default=levels);",
      "Formula (default=initial);",
      "Coefficient (default=parameter);",
      "Variable DFX # default levels change #;",
      "Formula DFX = 2;",
      "Variable (percent_change) DFY;",
      "Formula DFY = 3;",
      "Variable (linear) dfl;",
      "Coefficient DFP;",
      "Formula DFP = 1;",
      "Equation E_DFX DFX = DFY + DFP;",
      "Equation (linear) E_DFL dfl = c_DFX;",
      "Variable (default=linear);",
      "Equation (default=linear);",
      "Formula (default=always);",
      "Coefficient (default=non_parameter);",
      "Variable dfz;",
      "Equation E_DFZ dfz = dfl;",
      sep = "\n"
    )),
    quiet = TRUE,
    call = NULL
  ))
  stmt <- function(nm) model$tab[model$name %in% nm]
  expect_match(stmt("DFX")[1], "^Variable \\(levels,change\\) DFX")
  expect_match(stmt("DFY")[1], "^Variable \\(percent_change,levels\\) DFY")
  expect_match(stmt("dfl"), "^Variable \\(linear,change\\) dfl")
  expect_match(stmt("DFP")[1], "^Coefficient \\(parameter\\) DFP")
  expect_match(stmt("E_DFX"), "^Equation \\(levels\\) E_DFX")
  expect_match(stmt("E_DFL"), "^Equation \\(linear\\) E_DFL")
  expect_match(stmt("dfz"), "^Variable \\(linear,change\\) dfz")
  expect_match(stmt("E_DFZ"), "^Equation \\(linear\\) E_DFZ")
  expect_false(any(grepl("\\(\\s*default", model$tab, ignore.case = TRUE)))
})

test_that("statement keywords are case-insensitive (GEMPACK manual 11.1.3)", {
  quiet_pivot(model <- .process_tablo(
    tab_file = mutate_tab(paste(
      "COEFFICIENT (all,r,REG) KWC(r);",
      "FORMULA (all,r,REG) KWC(r) = 1;",
      "VARIABLE (all,r,REG) kwv(r);",
      "EQUATION E_KWV (all,r,REG) kwv(r) = KWC(r) * qgdp(r);",
      sep = "\n"
    )),
    quiet = TRUE,
    call = NULL
  ))
  expect_identical(model$type[model$name %in% "E_KWV"], "Equation")
  expect_identical(model$type[model$name %in% "KWC"], "Coefficient")
  expect_match(model$tab[model$name %in% "E_KWV"], "^Equation E_KWV")
})

# sets (GEMPACK manual 10.1.1.1 / 10.1.2.1; solver set readers)

test_that("self-referential set expressions abort", {
  expect_preflight_error("Set SBAD = SBAD + COMM;")
})

test_that("undeclared set references abort", {
  # in a set expression
  expect_preflight_error("Set SND = COMM - MRGX;")
  # as a set-equality right-hand side
  expect_preflight_error("Set SEQ = NOPE;")
  # in a Subset statement
  expect_preflight_error("Subset REG is subset of NOPE2;")
})

test_that("set self-equality aborts", {
  expect_preflight_error("Set SSE = SSE;")
})

test_that("element ranges expand (manual 11.2.2)", {
  model <- quiet_pivot(.process_tablo(
    tab_file = mutate_tab(paste(
      "Set SRG (s9 - s11, x, ind008 - ind010);",
      "Set (intertemporal) TQ (p0 - p2);",
      sep = "\n"
    )),
    quiet = TRUE,
    call = NULL
  ))
  sets <- model[model$type == "Set", ]
  expect_identical(
    sets$definition[sets$name == "SRG"][[1]],
    c("s9", "s10", "s11", "x", "ind008", "ind009", "ind010")
  )
  # fixed elements: an ordinary set to R, the qualifier kept for the solver
  expect_identical(sets$definition[sets$name == "TQ"][[1]], c("p0", "p1", "p2"))
  expect_identical(sets$qualifier_list[sets$name == "TQ"], "(non_intertemporal)")
  expect_match(sets$tab[sets$name == "TQ"], "(intertemporal)", fixed = TRUE)
})

test_that("a set name glued to = and a DOS end-of-file mark parse", {
  model <- quiet_pivot(.process_tablo(
    tab_file = mutate_tab(paste0(
      "Set GLUED=(all,r,REG: VDB(\"food\",r) > 0);\n",
      "Set GLUE2=(all,c,COMM: VDFB(c,\"food\",\"usa\") > 0);\n",
      "\x1a"
    )),
    quiet = TRUE,
    call = NULL
  ))
  sets <- model[model$type == "Set", ]
  expect_true(all(c("GLUED", "GLUE2") %in% sets$name))
})

test_that("a Read with its header glued to the quote parses", {
  model <- quiet_pivot(.process_tablo(
    tab_file = mutate_tab(paste0(
      "Coefficient (all,r,REG) GLH(r) # glued header #;\n",
      "Read GLH from file GTAPDATA header\"GLHD\";\n"
    )),
    quiet = TRUE,
    call = NULL
  ))
  rd <- model[model$type == "Read" & model$name %in% "GLH", ]
  expect_identical(rd$header, "GLHD")
  expect_identical(rd$file, "GTAPDATA")
})

test_that("malformed element ranges abort", {
  expect_preflight_error("Set SRG (s1 - t5);")
  expect_preflight_error("Set SRG (ind01 - ind123);")
  expect_preflight_error("Set SRG (s5 - s1);")
  expect_preflight_error("Set SRG (s1 - s2 - s3);")
})

test_that("malformed element lists abort", {
  expect_preflight_error("Set SEL ();")
  expect_preflight_error("Set SEL2 (x1,);")
})

test_that("over-length set headers abort", {
  expect_preflight_error(
    "Set SHD read elements from file GTAPSETS header \"TOOBIG\";"
  )
})

# unsupported statement forms

test_that("math statements without = abort", {
  # a plainly malformed Formula
  expect_preflight_error("Formula NOEQ 1;")
  # an unrecognized keyword surfaces as a math statement missing = (it
  # used to crash the extract parsers with a raw purrr error); grafted
  # onto a Formula explicitly, since an implicit continuation folds
  # into whatever statement precedes it in the fixture
  expect_preflight_error("Formula Frobnicate all the things;")
})

# Formula & Equation is supported since C0 (split into its 10.9.1
# halves by .check_statements) -- see test-tab_levels.R

# reads (GEMPACK manual 10.6/11.11.8)

test_that("headerless reads abort", {
  expect_preflight_error(
    "Coefficient (all,r,REG) ELX(r);\nRead ELX from file GTAPDATA;"
  )
})

test_that("reads into undeclared names abort", {
  expect_preflight_error(
    'Read NOTDECL from file GTAPDATA header "XXXX";'
  )
})

test_that("read from terminal aborts", {
  expect_snapshot_error(
    .chk_raw_reads("Read ELX from terminal", call = NULL)
  )
})

# PostSim rules (GEMPACK manual 12.2.1-12.2.3)

ps_wrap <- function(...) {
  paste(
    "PostSim (Begin);",
    ...,
    "PostSim (End);",
    sep = "\n"
  )
}

test_that("PostSim reads into PostSim coefficients pass", {
  txt <- ps_wrap(
    "Coefficient PSREADC # ps target #;",
    "File PSDATA;",
    'Read PSREADC from file PSDATA header "PSRD";'
  )
  quiet_pivot(expect_no_error(
    .process_tablo(tab_file = mutate_tab(txt), quiet = TRUE, call = NULL)
  ))
})

test_that("PostSim scope violations abort", {
  txt <- paste0(
    ps_wrap(
      "Coefficient PSCALC # ps #;",
      "Formula PSCALC = sum(r,REG, VKB(r));"
    ),
    "\nCoefficient ORDX # ord #;\nFormula ORDX = PSCALC + 1;"
  )
  expect_preflight_error(txt)
})

test_that("PostSim reads from ordinary files abort", {
  txt <- ps_wrap(
    "Coefficient PSREADC # ps target #;",
    'Read PSREADC from file GTAPDATA header "SAVE";'
  )
  expect_preflight_error(txt)
})

test_that("PostSim reads into ordinary coefficients abort", {
  txt <- ps_wrap(
    "File PSDATA;",
    'Read SAVE from file PSDATA header "PSRD";'
  )
  expect_preflight_error(txt)
})

test_that("PostSim reads into variables abort", {
  txt <- ps_wrap(
    "File PSDATA;",
    'Read psave from file PSDATA header "PSRD";'
  )
  expect_preflight_error(txt)
})

test_that("PostSim reads into undeclared names abort", {
  txt <- ps_wrap(
    "File PSDATA;",
    'Read NOTDECL from file PSDATA header "PSRD";'
  )
  expect_preflight_error(txt)
})

test_that("PostSim formulas assigning variables abort", {
  txt <- ps_wrap(
    "Formula (all,r,REG) psave(r) = 1;"
  )
  expect_preflight_error(txt)
})

test_that("PostSim formulas assigning ordinary coefficients abort", {
  txt <- ps_wrap(
    "Formula (all,r,REG) SAVE(r) = 1;"
  )
  expect_preflight_error(txt)
})

# regression: the shipped models pass the pre-flight unchanged

test_that("internal models pass the pre-flight", {
  tabs <- c(
    system.file("models/GTAPv7/GTAPv7.tab", package = "teems"),
    system.file("models/GTAP-RE/GTAP-RE.tab", package = "teems")
  )
  for (tab in tabs[nzchar(tabs)]) {
    quiet_pivot(expect_no_error(
      .process_tablo(tab_file = tab, quiet = TRUE, call = NULL)
    ))
  }
})

# fuzz batch 13 (2026-09-14): shapes the solver used to fault on
test_that("malformed quantifiers, sums and zerodivide defaults abort", {
  expect_preflight_error("Variable (all,r) qbad(r);")
  expect_preflight_error("Equation E_qbad2 (all,REG) qgdp(REG) = 0;")
  expect_preflight_error(
    "Coefficient (all,r,REG) CBAD(r);\nFormula (all,r,REG) CBAD(r) = sum(,REG, VGDP(r));"
  )
  expect_preflight_error(paste0(
    "Variable ", paste0("(all,d", 1:11, ",REG)", collapse = ""),
    " vbig(", paste0("d", 1:11, collapse = ","), ");"
  ))
  expect_preflight_error("Zerodivide (nonzero_by_zero) default NOSUCHCOEF;")
  expect_preflight_error(
    "Coefficient (all,r,REG) CBAD2(r);\nFormula (all,r,REG) CBAD2(r) = VGDP( );"
  )
  expect_preflight_error(
    "Coefficient (all,r,REG) CBAD3(r);\nFormula (all,r,REG) CBAD3(r) = VGDP(r,,);"
  )
})

test_that("statements over the solver statement buffer abort", {
  expect_preflight_error(paste0(
    "Coefficient (all,r,REG) CLONG(r);\nFormula (all,r,REG) CLONG(r) = VGDP(r)",
    strrep("+VGDP(r)", 2600), ";"
  ))
  # under the buffer as written, over it once each qgdp is prefixed p_
  expect_preflight_error(paste0(
    "Equation E_long (all,r,REG) qgdp(r) = 0", strrep("+qgdp(r)", 2100), ";"
  ))
})

test_that("unbalanced parentheses abort", {
  expect_preflight_error(
    "Coefficient (all,r,REG) CUNB(r);\nFormula (all,r,REG) CUNB(r) = sum(c,COMM, VDFP(c,\"food\",r);"
  )
  expect_preflight_error(
    "Coefficient (all,r,REG) CUNB2(r);\nFormula (all,r,REG) CUNB2(r) = (VGDP(r)+1));"
  )
})

test_that("subsets by numbers abort", {
  expect_snapshot_error(
    ems_model(
      mutate_tab("Subset (by_numbers) MARG is subset of COMM;", "by_numbers.tab"),
      model_files[["closure_file"]]
    )
  )
})

test_that("tab characters are blanks", {
  quiet_pivot(model <- .process_tablo(
    tab_file = mutate_tab("Coefficient\t(all,r,REG) TBX(r);\nFormula\t(all,r,REG)\tTBX(r) =\t2\n\t\t+ 1;"),
    quiet = TRUE,
    call = NULL
  ))
  expect_true("TBX" %in% model$name)
})

test_that("subsets by elements parse", {
  quiet_pivot(model <- .process_tablo(
    tab_file = mutate_tab(paste(
      "Set SMALL (food, crops);",
      "Subset (by_elements) SMALL is subset of COMM;",
      "Set SETA (c1 - c5);",
      "Set SETB (c3, c1, d5);",
      "Set SETC = SETA \\ SETB;",
      sep = "\n"
    )),
    quiet = TRUE,
    call = NULL
  ))
  expect_true(all(c("SMALL", "SETC") %in% model$name))
})

test_that("a qualifier list that never closes aborts", {
  expect_preflight_error("Coefficient (parameter QUNB;")
})

# set builders (GEMPACK manual 10.1.2)

test_that("intertemporal set builders abort", {
  expect_preflight_error("Set (intertemporal) BIGR = (all,r,REG: SAVE(r) > 0);")
})

test_that("a mapping-sum set builder over a non-mapping aborts", {
  expect_preflight_error(
    "Set COMMZ = (all,c,COMM: sum{a,ACTS: NOMAP(a) = c, SAVE(a)} > 0);"
  )
})

test_that("a binary switch in a set definition aborts", {
  expect_snapshot_error(
    ems_model(
      mutate_tab("Set ENDWX = (ENDW: ENDOWFLAG);", "binary_switch.tab"),
      model_files[["closure_file"]]
    )
  )
})

test_that("an IF in a Formula without quantifiers lowers through a one-element frame", {
  quiet_pivot(model <- .process_tablo(
    tab_file = mutate_tab(paste(
      "Coefficient SIFA # scalar IF target #;",
      "Formula SIFA = 1 + IF(sum{r,REG,VKB(r)} > 1, 2) + IF(SIFA = 0, 5);",
      "Coefficient SIFB;",
      "Formula (initial) SIFB = IF(SIFA > 0, SIFA);",
      sep = "\n"
    )),
    quiet = TRUE,
    call = NULL
  ))
  frame <- model$tab[grepl("^Set IFO1 ", model$tab)]
  expect_length(frame, 1L)
  expect_match(frame, "(ifo1e)", fixed = TRUE)
  expect_match(
    model$tab,
    "Formula (all,ifo1i,IFO1: IFX1 > 1) IFV1(ifo1i) = IFV1(ifo1i) + [2];",
    fixed = TRUE, all = FALSE
  )
  expect_match(
    model$tab,
    "Formula (all,ifo1i,IFO1: SIFA = 0) IFV1(ifo1i) = IFV1(ifo1i) + [5];",
    fixed = TRUE, all = FALSE
  )
  expect_match(model$tab, "Formula SIFA = sum(ifo1i,IFO1,IFV1(ifo1i));", fixed = TRUE, all = FALSE)
  expect_match(model$tab, "Formula (initial) SIFB = sum(ifo1i,IFO1,IFV2(ifo1i));", fixed = TRUE, all = FALSE)
  expect_false(any(grepl("\\bIF\\s*\\(", model$tab)))
})

test_that("an IF over a scalar coefficient in an Equation lowers to a scalar indicator (SIMPLEv3 SEGMKT)", {
  quiet_pivot(model <- .process_tablo(
    tab_file = mutate_tab(paste(
      "Coefficient (parameter) SWP # parameter switch #;",
      "Formula SWP = 1;",
      "Coefficient SWN # non-parameter switch #;",
      "Formula SWN = 1;",
      "Variable (all,r,REG) zsw(r);",
      "Equation E_zsw (all,r,REG) zsw(r) = IF{SWP = 1, qgdp(r)} + IF{SWP = 0, pop(r)} + IF{SWN > 0, pop(r)};",
      "Variable (levels) LSW;",
      "Formula (initial) LSW = 2;",
      "Equation (levels) E_LSW IF{SWP = 1, LSW} + IF{SWP = 0, 2*LSW} = 2;",
      sep = "\n"
    )),
    quiet = TRUE,
    call = NULL
  ))
  tab <- model$tab
  eq <- tab[grepl("^Equation E_zsw ", tab)]
  ind <- regmatches(eq, gregexpr("IFC[0-9]+", eq))[[1]]
  expect_length(unique(ind), 3L)
  expect_match(eq, paste(ind[1], "* qgdp(r)"), fixed = TRUE)
  decl <- function(i) tab[grepl(paste0("^Coefficient (\\(parameter\\) )?", i, " "), tab)]
  expect_identical(decl(ind[1]), paste0("Coefficient (parameter) ", ind[1], " # if-rewrite indicator SWP = 1 #;"))
  expect_identical(decl(ind[2]), paste0("Coefficient (parameter) ", ind[2], " # if-rewrite indicator SWP = 0 #;"))
  expect_identical(decl(ind[3]), paste0("Coefficient ", ind[3], " # if-rewrite indicator SWN > 0 #;"))
  frame <- tab[grepl("^Set IFO[0-9]+ ", tab)]
  expect_length(frame, 1L)
  fr <- sub("^Set (IFO[0-9]+) .*$", "\\1", frame)
  fi <- paste0(tolower(fr), "i")
  expect_match(tab, sprintf("Formula (all,%s,%s: SWP = 1) ", fi, fr), fixed = TRUE, all = FALSE)
  expect_match(tab, sprintf("Formula %s = sum(%s,%s,", ind[1], fi, fr), fixed = TRUE, all = FALSE)
  # a levels equation keeps its IF terms: the solver linearizes them
  # (d IF[c, A] = IF[c, dA], manual 11.4.6)
  lev <- tab[grepl("^Equation \\(levels\\) E_LSW ", tab)]
  expect_match(lev, "IF[SWP = 1, LSW] + IF[SWP = 0, 2*LSW]", fixed = TRUE)
  expect_false(any(grepl("\\bIF\\s*[({]", tab[grepl("^(Equation|Formula)", tab)])))
})

test_that(".tab_coef_is_param reads parameter, integer and non_parameter qualifiers", {
  tab <- c(
    "Coefficient (parameter) A # a #;",
    "Coefficient (integer) (all,r,REG) B(r);",
    "Coefficient (integer, non_parameter) C;",
    "Coefficient (all,r,REG) D(r);"
  )
  expect_true(.tab_coef_is_param(tab, "a"))
  expect_true(.tab_coef_is_param(tab, "B"))
  expect_false(.tab_coef_is_param(tab, "C"))
  expect_false(.tab_coef_is_param(tab, "D"))
  expect_false(.tab_coef_is_param(tab, "E"))
})

test_that("condensation keeps a quantifier index whose case differs from its uses (GEMPACK manual 11.1.3)", {
  quiet_pivot(model <- suppressWarnings(.process_tablo(
    tab_file = mutate_tab(paste(
      "Variable (all,c,COMM)(all,r,REG) zqw(c,r);",
      "Equation E_zqw (all,C,COMM)(all,r,REG) zqw(c,r) = sum{d,REG, VFOB(c,r,d)*pfob(c,r,d)};",
      sep = "\n"
    )),
    backsolve = "pfob",
    quiet = TRUE,
    call = NULL
  )))
  eq <- model$tab[model$name %in% "E_zqw"]
  csub <- regmatches(eq, regexpr("CSUB[0-9]+", eq))
  expect_match(eq, paste0(csub, "(c,r)*pds(c,r)"), fixed = TRUE)
  expect_match(
    model$tab,
    paste0("Formula (all,c,COMM)(all,r,REG) ", csub, "(c,r) = sum{d,REG, VFOB(c,r,d)}"),
    fixed = TRUE, all = FALSE
  )
})

test_that("an IN condition rebinds its index whatever the case it is written in", {
  r <- .if_in_rebind("x(r) = IF(R in SUB, V(r) + W(R))")
  expect_identical(r$text, "x(r) = IF(R in SUB, V(R@in1) + W(R@in1))")
  expect_identical(unname(r$extra), "SUB")
})

test_that("the raw-TAB subset closure reads Subset (by_elements) and the relative complement", {
  sup <- .tab_subset_closure(c(
    "Subset (by_elements) A is subset of B",
    "Set C = B \\ A",
    "Set D = B - A"
  ))
  expect_identical(sup$A, "B")
  expect_identical(sup$C, "B")
  expect_identical(sup$D, "B")
})
