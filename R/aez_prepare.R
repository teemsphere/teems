#' GTAP-AEZ database preparation (GTAP_convert target "GTAP-AEZ";
#' applied by ems_data when the layer is detected)
#'
#' The AEZ layer ships on a GTAPv7-format database (GTAPv12a, v11c)
#' with the sets AEZS, COVS, CROP, LUSA and the data AREA/TONS (AEZS x
#' CROP x REG), LCOV (AEZS x COVS x REG); the v6-format layer of
#' GTAPv10a uses other headers and is not prepared. The GTAP-AEZ
#' model declares
#'   - the disaggregated activity sets DACTS (DACT), DCROPACTS (DCRP),
#'     DFORSACTS (DFRS), DGRAZACTS (DGRZ), DLANDACTS (DLUA) and the
#'     mapping MACTS (MACT) from DACTS to ACTS, which flexagg's
#'     aggdat_aez.tab synthesizes and the database does not carry:
#'     DACT and MACT are the activity list at source resolution (the
#'     mapping composes onto the aggregated ACTS at deploy,
#'     .finalize_map_data), DCRP = CROP, DFRS = frs, DGRZ = ctl rmk
#'     wol, DLUA = LUSA;
#'   - AREA/TONS over CROPACTS and LCOV over LCOV: those dimensions
#'     are renamed (the CROP and COVS sets take the model names
#'     CROPACTS/LCOV so the aggregation reaches them);
#'   - the AEZ parameters EAEZ, ETAE, YDON, YDRS, YDET: assigned the
#'     flexagg constants (ESUBAEZ 20 on land-using activities, ETAE
#'     0.66, YDON 1 on crop activities, YDRS 1, YDET 0.25) when absent.
#' The disaggregated sets are marked user sets: they never aggregate.
#'
#' @keywords internal
#' @noRd
.is_aez_input <- function(i_data) {
  nm <- toupper(names(i_data))
  all(c("AEZS", "CROP", "LUSA") %in% nm) && any(c("AREA", "TONS", "LCOV") %in% nm)
}

#' @importFrom purrr map_lgl
#' @keywords internal
#' @noRd
.prepare_aez <- function(i_data, call) {
  metadata <- attr(i_data, "metadata")
  fmt <- metadata$data_format
  cls <- class(i_data)
  nm <- toupper(names(i_data))
  req <- c("AEZS", "COVS", "CROP", "LUSA", "ACTS", "REG", "AREA", "TONS", "LCOV")
  missing_aez <- setdiff(req, nm)
  if (length(missing_aez) > 0L) {
    .cli_action(data_err$aez_incomplete,
      action = "abort",
      call = call
    )
  }
  set_of <- function(h) tolower(trimws(as.character(i_data[[match(h, nm)]])))
  acts <- set_of("ACTS")
  crop <- set_of("CROP")
  lusa <- set_of("LUSA")

  mk_set <- function(header, ele) {
    s <- ele
    class(s) <- c(header, header, "set", fmt, "character")
    attr(s, "user_set") <- TRUE
    s
  }
  new_sets <- list(
    DACT = mk_set("DACT", acts),
    DCRP = mk_set("DCRP", intersect(crop, acts)),
    DFRS = mk_set("DFRS", intersect("frs", acts)),
    DGRZ = mk_set("DGRZ", intersect(c("ctl", "rmk", "wol"), acts)),
    DLUA = mk_set("DLUA", intersect(lusa, acts)),
    MACT = mk_set("MACT", acts)
  )
  new_sets <- new_sets[!names(new_sets) %in% nm]

  # the CROP and COVS sets carry the model's set names so dimensions
  # renamed onto them keep aggregating
  rename_set <- function(header, name) {
    i <- match(header, nm)
    s <- i_data[[i]]
    class(s)[2] <- name
    i_data[[i]] <<- s
  }
  rename_set("CROP", "CROPACTS")
  rename_set("COVS", "LCOV")
  rename_dim <- function(header, from, to) {
    i <- match(header, nm)
    a <- i_data[[i]]
    dn <- names(dimnames(a))
    if (!is.null(dn)) {
      dn[toupper(dn) == from] <- to
      names(dimnames(a)) <- dn
    }
    i_data[[i]] <<- a
  }
  rename_dim("AREA", "CROP", "CROPACTS")
  rename_dim("TONS", "CROP", "CROPACTS")
  rename_dim("LCOV", "COVS", "LCOV")

  reg <- set_of("REG")
  aezs <- set_of("AEZS")
  mk_par <- function(header, dims, value) {
    if (length(dims) == 0L) {
      arr <- array(value, dim = 1L)
    } else {
      arr <- array(value, dim = lengths(dims), dimnames = dims)
    }
    class(arr) <- c(header, "par", fmt, class(arr))
    arr
  }
  new_par <- list(
    EAEZ = mk_par("EAEZ", list(ACTS = acts, REG = reg), ifelse(acts %in% lusa, 20, 0)),
    ETAE = mk_par("ETAE", list(AEZS = aezs, REG = reg), 0.66),
    YDON = mk_par("YDON", list(ACTS = acts, REG = reg), ifelse(acts %in% crop, 1, 0)),
    YDRS = mk_par("YDRS", list(REG = reg), 1),
    YDET = mk_par("YDET", list(), 0.25)
  )
  new_par <- new_par[!names(new_par) %in% nm]

  # keep the set / par / dat grouping: new sets after the last set,
  # new parameters after the last parameter
  attrs <- attributes(i_data)
  out <- unclass(i_data)
  at_set <- max(which(purrr::map_lgl(out, inherits, "set")), 0L)
  out <- append(out, new_sets, after = at_set)
  at_par <- max(which(purrr::map_lgl(out, inherits, "par")), 0L)
  out <- append(out, new_par, after = at_par)
  for (a in setdiff(names(attrs), c("names", "class", "metadata"))) {
    attr(out, a) <- attrs[[a]]
  }
  metadata$aez <- TRUE
  attr(out, "metadata") <- metadata
  class(out) <- cls
  out
}
