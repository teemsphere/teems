#' The calibrated constants behind the recommendation, each entry
#' carrying its provenance (printed with the recommendation).
#' @keywords internal
#' @noRd
.auto_thresholds <- function() {
  thresholds <- list(
    # ---- probe policy -------------------------------------------------
    # plain static: the probe costs 2.3-2.9x a Johansen solve at
    # 346k-534k eq (9-11 s) but 10.7-15x at 1.41M and 27-50x from 3.46M
    # (more than the whole Gragg solve it would inform), super-linear
    # in size; above this it is skipped and the metadata rule decides
    probe_plain_max = 1e6,
    # condensed static: always cheap (3.7-44 s to 230k condensed eq,
    # 0.3-1.1x a Johansen), so it always runs when a bordered method
    # is a candidate; intertemporal: never (SBBD is fixed by structure)
    # ---- static crossovers --------------------------------------------
    # plain static: DBBD beats LU at every rung 346k-7.69M under Gragg
    # (DBBD/LU 0.62 -> 0.30) and ties or wins under Johansen (ties below
    # ~600k, whole solves under 10 s) -- no size gate
    dbbd_hint_min = 3e5,
    # condensed static, multi-step: threaded LU wins to 110k condensed eq
    # (DBBD/LU 2.0 -> 1.12), ties at 140k (0.99/0.87)
    dbbd_condensed_size = 1.2e5,
    # condensed static, Johansen: DBBD wins from 70k condensed eq
    # (0.61 -> 0.03 at 230k)
    dbbd_condensed_size_johansen = 7e4,
    # ceiling on max(border variables, border equations) / system size
    # for a bordered method chosen from probe evidence; never binds on
    # GTAP geometry (2.8e-4 S-full, 3.9e-5 I-200) -- a guard
    border_share_max = 0.10,
    # ---- ranks and threads ----------------------------------------------
    # SBBD: 1->2 ranks -26..-33 %, 2->4 -3..-21 %, 4->8 -11..+1 % on the
    # ladder; ranks beat threads at every fixed core count (1x8 is +55..
    # +93 % against 8x1); on the box 8x4 beats 32x1 (I-long) -- knee 4,
    # cap 8, the remaining cores go to threads
    ranks_sbbd_max = 8L,
    # DBBD plain: flat from 2 ranks at 8 cores (S14P 171/170/170 s at
    # 2x4/4x2/8x1) while each rank costs +0.27 kB/eq; condensed DBBD gets
    # slower with ranks (S14C 153 -> 198 -> 282 s) and OOMs at 8 from
    # 110k condensed eq -- 2 ranks on a laptop, up to 8 where the core
    # count and the memory model allow (S-full 4->8 ranks 1.56x)
    ranks_dbbd_laptop = 2L,
    ranks_dbbd_max = 8L,
    cores_laptop_max = 8L,
    # t8 no better than t4 at the knee (I-long SBBD8: 496 vs 506 s)
    threads_max = 8L,
    # ---- LU workspace ceiling (unchanged; MA48_LA_MAX, 32-bit HSL) ------
    lu_la_ceiling = 2147483647,
    # measured LA / nnz (2026-08): I-long 6.0, S-full 12.0, S-full-cond 40
    lu_fill = 12,
    lu_fill_condensed = 40,
    lu_ceiling_warn_share = 0.75,
    # ---- memory model: peak GB = kB/eq x plain-equivalent equations ------
    # plain-equivalent = solved system + backsolved elements: condensation
    # cuts equations ~20x but not peak memory (the value tables follow
    # the data); measured whole-container peaks, Gragg, ladder 6.3
    # LU@1: 0.58-0.70 kB/eq to 3.77M, 0.84-0.90 at 7.69M
    mem_lu = 0.90,
    # condensed LU = 0.90-1.21x the plain rig's LU
    mem_lu_condensed = 1.2,
    # DBBD: eq x (0.85 + 0.27 x ranks) kB, anchored on the binding cell
    # S90P DBBD2 (10.7 predicted vs 10.8-11.2 GB), 7-25 % over on smaller
    # rungs -- conservative in the right direction
    mem_dbbd_base = 0.85,
    mem_dbbd_rank = 0.27,
    # condensed DBBD = 1.30-1.55x the plain rig's DBBD
    mem_dbbd_condensed = 1.55,
    # SBBD: (0.36 + 0.02 x ranks) kB/eq at >= 4.5M; extrapolates to
    # I-long-big 21.9M within 15 %
    mem_sbbd_base = 0.36,
    mem_sbbd_rank = 0.02,
    # NDBBD: replicated tables 0.148 GB per M eq per rank (34.7 GB at
    # Q34's 234M, one rank) plus the local matrix share; the per-thread
    # working sets are capped by the solver itself (dev8 thread budget)
    mem_ndbbd_rank = 0.15,
    mem_ndbbd_local = 0.05,
    # a choice must fit inside this share of the container's memory
    mem_fit_share = 0.90,
    # the won't-fit abort fires only past the model's error band: an
    # estimate above this multiple of the limit cannot be a false alarm
    mem_abort_ratio = 1.2
  )
  return(thresholds)
}
