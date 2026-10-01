# Model layers -------------------------------------------------------
#
# The GTAP model layers (AEZ, E, Power) that ems_data() and
# GTAP_convert() prepare before the core pipeline sees the data. Each
# entry is the single description of one layer: how to detect it in a
# loaded database, what the input must carry, which headers the
# preparation synthesises and with what flags, and the element
# vocabulary the model's .tab expects.
#
# The .tab is the authority here -- .prepare_aez()/.prepare_e()/
# .prepare_ep() exist only to synthesise the headers the model reads --
# so check_layer_spec() re-derives the inventory from
# inst/models/<model>/<model>.tab and refuses to write sysdata when the
# two disagree. The .cli_action() messages that name these headers are
# generated from the same entries (build_data_err, build_data_info), so
# the spec, the code and the prose cannot drift apart.

build_layer_spec <- function() {
  spec <- list(
    aez = list(
      flag = "aez",
      model = "GTAP-AEZ",
      target = "GTAP-AEZ",
      label = "GTAP-AEZ",
      detect = list(
        all = c("AEZS", "CROP", "LUSA"),
        any = c("AREA", "TONS", "LCOV"),
        none = character()
      ),
      required = list(
        set = c("AEZS", "COVS", "CROP", "LUSA", "ACTS", "REG"),
        par = character(),
        dat = c("AREA", "TONS", "LCOV")
      ),
      # every AEZ set is its named elements restricted to the
      # activities the aggregation actually carries
      restrict = "ACTS",
      sets = list(
        DACT = list(header = "ACTS", group = "activity"),
        DCRP = list(header = "CROP", group = "activity"),
        DFRS = list(elements = "frs", group = "activity"),
        DGRZ = list(elements = c("ctl", "rmk", "wol"), group = "activity"),
        DLUA = list(header = "LUSA", group = "activity"),
        MACT = list(header = "ACTS", group = "activity")
      ),
      # defaults for the AEZ parameters the .tab reads but no database
      # carries; `on` names the set whose membership picks `hi` over
      # `lo` across the first dimension, NA means the constant `hi`
      par = list(
        EAEZ = list(dims = c("ACTS", "REG"), on = "LUSA", hi = 20, lo = 0),
        ETAE = list(dims = c("AEZS", "REG"), on = NA, hi = 0.66, lo = NA),
        YDON = list(dims = c("ACTS", "REG"), on = "CROP", hi = 1, lo = 0),
        YDRS = list(dims = "REG", on = NA, hi = 1, lo = NA),
        YDET = list(dims = character(), on = NA, hi = 0.25, lo = NA)
      ),
      rename_set = c(CROP = "CROPACTS", COVS = "LCOV"),
      rename_dim = list(
        AREA = c(from = "CROP", to = "CROPACTS"),
        TONS = c(from = "CROP", to = "CROPACTS"),
        LCOV = c(from = "COVS", to = "LCOV")
      ),
      # the .tab declares CROPACTS and LCOV as subsets, so no header
      # of this layer is synthesised for R alone
      r_only = character(),
      # no family grammar to close over: the six activity sets are an
      # enumeration, so listing them here would only restate `sets`
      owns = NA_character_
    ),
    e = list(
      flag = "e",
      model = "GTAP-E",
      target = "GTAP-E",
      label = "GTAP-E",
      detect = list(
        all = c("COME", "SUBE", "INCE"),
        any = character(),
        none = c("ELEA", "ELEC", "POWR")
      ),
      required = list(
        set = c("COMM", "ACTS", "REG", "COME", "FUEL"),
        par = c("SUBE", "INCE"),
        dat = character()
      ),
      # natural_resource: the sector-specific endowment (the .tab's
      # ENDWF), whose cost share recalibrates ELFVAEN for fossil mining
      vocab = list(energy_top = "eny", natural_resource = "natlres"),
      sets = list(
        DCOM = list(group = "commodity"),
        MCOM = list(group = "commodity"),
        DELY = list(group = "commodity"),
        EGY = list(group = "energy", user_set = FALSE),
        ENY = list(group = "energy", user_set = FALSE, agents = c("P", "G", "I")),
        TOPP = list(group = "top", user_set = FALSE)
      ),
      reclass = c(TRBL = FALSE, MAPB = TRUE),
      cde = list(
        dim = "TOPP",
        pairs = list(
          SUBE = list(to = "SUBP", max = 1),
          INCE = list(to = "INCP", max = NA)
        )
      ),
      # TOPP is computed in the .tab (ENY + NENYP), never read; the R
      # side still needs it to dimension the promoted CDE parameters
      r_only = "TOPP",
      owns = "^ENY[FGIP]$"
    ),
    ep = list(
      flag = "ep",
      model = "GTAP-EP",
      target = "GTAP-EP",
      label = "GTAP-Power",
      detect = list(
        all = c("COME", "SUBE", "INCE", "ELEA", "ELEC", "POWR"),
        any = character(),
        none = character()
      ),
      required = list(
        set = c("COMM", "ACTS", "REG", "COME", "FUEL", "ELEC", "ELEA"),
        par = c("SUBE", "INCE"),
        dat = character()
      ),
      vocab = list(
        energy_top = "eny",
        natural_resource = "natlres",
        electricity = "ely",
        generation = "egen",
        base_load = "ebl",
        peak_load = "epl"
      ),
      sets = list(
        DCOM = list(group = "commodity"),
        MCOM = list(group = "commodity"),
        DELY = list(group = "commodity"),
        EGY = list(group = "energy", user_set = FALSE),
        TOPP = list(group = "top", user_set = FALSE),
        ENY = list(group = "energy", user_set = FALSE, agents = c("P", "G", "I"))
      ),
      # the electricity nest: one header per family and agent, expanded
      # agent-major to match the order the .tab reads them in
      nest = list(
        agents = c("F", "G", "I", "P"),
        families = list(
          ELE = list(user_set = TRUE),
          ELY = list(user_set = TRUE),
          EGN = list(user_set = TRUE),
          EBL = list(user_set = FALSE),
          EPL = list(user_set = FALSE)
        )
      ),
      reclass = c(TRBL = FALSE, MAPB = TRUE),
      cde = list(
        dim = "TOPP",
        pairs = list(
          SUBE = list(to = "SUBP", max = 1),
          INCE = list(to = "INCP", max = NA)
        )
      ),
      r_only = "TOPP",
      owns = "^(ELE|ELY|EGN|EBL|EPL|ENY)[FGIP]$"
    )
  )
  lapply(spec, expand_layer_headers)
}

# Flattens `sets` and `nest` into the ordered header inventory the R
# side builds from and the check compares against. `family` maps each
# expanded header back to the `sets`/`nest` entry that produced it, so
# .prepare_*() supplies one element vector per family, not per header.
expand_layer_headers <- function(spec) {
  headers <- character()
  family <- character()
  user_set <- logical()
  group <- character()

  add <- function(h, f, u, g) {
    headers <<- c(headers, h)
    family <<- c(family, rep(f, length(h)))
    user_set <<- c(user_set, rep(u, length(h)))
    group <<- c(group, rep(g, length(h)))
  }

  for (key in names(spec$sets)) {
    s <- spec$sets[[key]]
    u <- if (is.null(s$user_set)) TRUE else s$user_set
    h <- if (is.null(s$agents)) key else paste0(key, s$agents)
    add(h, key, u, s$group)
  }
  if (!is.null(spec$nest)) {
    for (agent in spec$nest$agents) {
      for (key in names(spec$nest$families)) {
        add(paste0(key, agent), key, spec$nest$families[[key]]$user_set, "nest")
      }
    }
  }

  spec$headers <- headers
  spec$family <- stats::setNames(family, headers)
  spec$user_set <- stats::setNames(user_set, headers)
  spec$group <- stats::setNames(group, headers)
  spec
}

# Every header a layer synthesises must be one its model actually
# reads. Where the headers follow a family x agent grammar the check
# closes both ways, so a .tab that grows an agent or a family fails the
# build instead of silently going unprepared.
check_layer_spec <- function(layer_spec, models_dir = "./inst/models") {
  for (spec in layer_spec) {
    tab <- file.path(models_dir, spec$model, paste0(spec$model, ".tab"))
    if (!file.exists(tab)) {
      stop("layer_spec: no .tab for model ", spec$model, call. = FALSE)
    }
    src <- readLines(tab, warn = FALSE)
    tab_read <- function(file) {
      pat <- paste0(file, ' +header +"[A-Za-z0-9]+"')
      hit <- unlist(regmatches(src, gregexpr(pat, src, ignore.case = TRUE)))
      unique(toupper(gsub('.*"([A-Za-z0-9]+)".*', "\\1", hit)))
    }
    tab_sets <- tab_read("GTAPSETS")
    tab_par <- tab_read("GTAPPARM")

    unread <- setdiff(setdiff(spec$headers, spec$r_only), tab_sets)
    if (length(unread) > 0L) {
      stop(
        "layer_spec ", spec$flag, ": set header(s) ",
        paste(unread, collapse = ", "), " are not read by ",
        basename(tab), call. = FALSE
      )
    }
    unread <- setdiff(names(spec$par), tab_par)
    if (length(unread) > 0L) {
      stop(
        "layer_spec ", spec$flag, ": parameter header(s) ",
        paste(unread, collapse = ", "), " are not read by ",
        basename(tab), call. = FALSE
      )
    }
    if (!is.na(spec$owns)) {
      in_tab <- grep(spec$owns, tab_sets, value = TRUE)
      in_spec <- grep(spec$owns, spec$headers, value = TRUE)
      if (!setequal(in_tab, in_spec)) {
        unmade <- sort(setdiff(in_tab, in_spec))
        unread <- sort(setdiff(in_spec, in_tab))
        stop(
          "layer_spec ", spec$flag, ": ",
          paste(c(
            if (length(unmade) > 0L) {
              paste0(
                basename(tab), " reads ", paste(unmade, collapse = ", "),
                ", which the spec does not synthesise"
              )
            },
            if (length(unread) > 0L) {
              paste0(
                "the spec synthesises ", paste(unread, collapse = ", "),
                ", which ", basename(tab), " does not read"
              )
            }
          ), collapse = "; "),
          call. = FALSE
        )
      }
    }
  }
  invisible(TRUE)
}

# "A", "A and B", "A, B and C" -- the form the layer messages use
layer_and <- function(x) {
  if (length(x) < 2L) {
    return(paste0(x, collapse = ""))
  }
  paste(paste(x[-length(x)], collapse = ", "), "and", x[[length(x)]])
}

layer_group <- function(spec, group) {
  paste(spec$headers[spec$group == group], collapse = ", ")
}
