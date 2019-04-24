#' Find AB
#' @noRd
#' @importFrom dplyr mutate right_join
#' @importFrom rlang !!
#' @param AG AG
#' @param B B
#' @examples
#' library(tibble)
#' AG <- tribble(
#'   ~elm,  ~phase3,     ~g,    ~g_se,           ~a ,       ~a_se,
#'   "Mg",     "Ol", 5797.0, 16.15739, 0.0001011356, 1.200497e-14,
#'   "Si",     "Ol", 4282.5, 10.12659, 0.0001011270, 1.129788e-14,
#'   "Si",    "Qtz", 9891.0, 14.91865, 0.0001009895, 3.593501e-15,
#'   "Mg",    "Qtz",    0.0,  0.00000, 0.0001011356, 5.848577e-15,
#' )
#' B <- tribble(
#'   ~elm, ~stg,        ~b,        ~b_se,
#'   "Mg",   11,  99.75446, 0.0004009793,
#'   "Si",   11,  99.88468, 0.0003773498
#' )
#' find(AG, B)
#' #   elm stg  phase3        ab        ab_se
#' # 1  Mg  11     Ol 0.01008873 4.055329e-08
#' # 2  Mg  11    Qtz 0.01008873 4.055329e-08
#' # 3  Si  11     Ol 0.01010103 3.816024e-08
#' # 4  Si  11    Qtz 0.01008730 3.810836e-08
find_AB <- function(AG, B, se = TRUE) {
  mutate(
    right_join(AG[, c("elm", "phase3", "a", "a_se"[se])], B, by = "elm"),
    ab = a * b,
    ab_se = if (!!se) L2(a * b_se, b * a_se) else NA_real_,
    a = NULL, a_se = NULL, b = NULL, b_se = NULL
  )
}

#' Expand AB along stg
#' @noRd
#' @param AB AB
#' @param stg stg
#'
#' @importFrom tidyr gather spread
#' @importFrom dplyr right_join select
#' @note
#' > AB
#'   elm stg phase3         ab        ab_se
#' 1  Mg  11     Ol 0.01008873 4.055329e-08
#' 2  Mg  11    Qtz 0.01008873 4.055329e-08
#' 3  Si  11     Ol 0.01010103 3.816024e-08
#' 4  Si  11    Qtz 0.01008730 3.810836e-08
#' > stg
#' c("11", "11")
#' > expand_AB(AB, stg)
#' List of 2
#' $ Mg:List of 2
#' ..$ ab   :'data.frame':	2 obs. of  2 variables:
#'   .. ..$ Ol : num [1:2] 0.0101 0.0101
#' .. ..$ Qtz: num [1:2] 0.0101 0.0101
#' ..$ ab_se:'data.frame':	2 obs. of  2 variables:
#'   .. ..$ Ol : num [1:2] 4.06e-08 4.06e-08
#' .. ..$ Qtz: num [1:2] 4.06e-08 4.06e-08
#' $ Si:List of 2
#' ..$ ab   :'data.frame':	2 obs. of  2 variables:
#'   .. ..$ Ol : num [1:2] 0.0101 0.0101
#' .. ..$ Qtz: num [1:2] 0.0101 0.0101
#' ..$ ab_se:'data.frame':	2 obs. of  2 variables:
#'   .. ..$ Ol : num [1:2] 3.82e-08 3.82e-08
#' .. ..$ Qtz: num [1:2] 3.81e-08 3.81e-08
expand_AB <- function(AB, stg) {
  .stg <- data.frame(stg = stg)
  gather(AB, .var, .val, -elm, -stg, -phase3) %>>%
    spread(phase3, .val) %>>%
    split(.$elm) %>>%
    lapply(function(x) lapply(split(x, x$.var), join_by_stg, .stg))
}

join_by_stg <- function(x, stg) {
  x %>>%
    select(-elm, -.var) %>>%
    right_join(stg, by = "stg") %>>%
    select(-stg)
}

# © 2018 JAMSTEC
#' Fix parameters alpha, beta, and gamma based on given chemical compositions.
#' @noRd
#'
#' @param xmap `qm_xmap` class object returned by [`read_xmap()`]
#' @param cls `qm_cluster` class object returned by [`cluster_xmap()`]
#' @param csv
#'   A file path to the csv file with columns `phase`, `oxide` and `wt`.
#'
#' @importFrom matrixStats rowMaxs weightedMedian
#' @importFrom dplyr
#'   filter group_by mutate right_join summarize
#' @importFrom rlang !!
#' @importFrom tidyr gather
fix_AB_by_wt <- function(xmap, cls, params) {
  if (!any(is.finite(params$wt))) return(NULL)
  params <- params[is.finite(params$wt), c("phase", "oxide", "wt")]
  xmap[(unique(params$oxide))] %>>%
    lapply(unlist, use.names = FALSE) %>>%
    c(list(phase = cls$cluster, w = rowMaxs(cls$membership))) %>>%
    as.data.frame(stringsAsFactors = FALSE) %>>%
    filter(phase %in% (!!params$phase)) %>>%
    gather(oxide, mapint, -phase, -w) %>>%
    group_by(phase, oxide) %>>%
    summarize(mapint = weightedMedian(mapint, w)) %>>%
    ungroup %>>%
    right_join(params, by = c("phase", "oxide")) %>>%
    mutate(ab = wt / mapint, mapint = NULL, wt = NULL) %>>%
    rename(elm = oxide, phase3 = phase)
}

# © 2018 JAMSTEC
#' Join results of find_AB and fix_AB_by_wt
#' @noRd
#' @param AB returned by find_AB
#' @param AB_fixed returnd by fix_AB_by_wt()
#'
#' @importFrom dplyr anti_join bind_rows left_join select semi_join
join_AB <- function(AB, AB_fixed = NULL) {
  if (is.null(AB_fixed)) return(AB)
  semi_join(AB, AB_fixed, by = c("elm", "phase3")) %>>%
    select(-ab, -ab_se) %>>%
    left_join(AB_fixed, by = c("elm", "phase3")) %>>%
    bind_rows(anti_join(AB, AB_fixed, by = c("elm", "phase3")))
}
