#' Find AB
#' @importFrom dplyr mutate right_join select
#' @importFrom purrr map 
#' @importFrom stats setNames
#' @importFrom tidyr nest spread unnest
#' @importFrom tibble tibble
#' @param A A
#' @param B B
#' @param stg stg
#' @noRd
# > AG
#   elm phase3      g     g_se            a         a_se
# 1  Mg     Ol 5797.0 16.15739 0.0001011356 1.200497e-14
# 2  Si     Ol 4282.5 10.12659 0.0001011270 1.129788e-14
# 3  Si    Qtz 9891.0 14.91865 0.0001009895 3.593501e-15
# 4  Mg    Qtz    0.0  0.00000 0.0001011356 5.848577e-15

# > B
#   elm stg        b         b_se
# 1  Mg  11 99.75446 0.0004009793
# 2  Si  11 99.88468 0.0003773498

# > AB
# elm stg phase3         ab        ab_se
# 1  Mg  11     Ol 0.01008873 4.055329e-08
# 2  Mg  11    Qtz 0.01008873 4.055329e-08
# 3  Si  11     Ol 0.01010103 3.816024e-08
# 4  Si  11    Qtz 0.01008730 3.810836e-08
find_AB <- function(AG, B) {
  mutate(
    right_join(
      AG[, c("elm", "phase3", "a", "a_se")],
      B, by = "elm"
    ),
    ab = a * b,
    ab_se = L2(a * b_se, b * a_se),
    a = NULL, a_se = NULL, b = NULL, b_se = NULL
  )
}

#' Expand AB along stg
#' @importFrom tidyr gather spread
#' @importFrom dplyr right_join select
#' @importFrom purrr map
#' @note
#' > AB
#' elm stg phase3         ab        ab_se
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
#' @noRd
expand_AB <- function(AB, stg) {pipeline({
  gather(AB, .var, .val, -elm, -stg, -phase3)
  spread(phase3, .val)
  split(.$elm)
  map(function(x) split(x, x$.var))
  map(map, select, -elm, -.var)
  map(map, right_join, data.frame(stg = stg), by = "stg")
  map(map, select, -stg)
})}



#' fix AB value when composition of certain phases are constant
#' @inheritParams find_AB
#' @param AB A list of parameters alpha and beta
#' @param fix 
#' Csv file indicating composition of the phases. `NULL`` returns input AB.
#' @param X Membership degrees.
#' @param fine_th A threshold of X
#' @importFrom purrr map map2_dbl
#' @importFrom matrixStats weightedMedian
#' @noRd
find_AB_fix <- function(AB, fix = NULL, X, fine_th = .90, xmap) {
  
  if(is.null(fix)) return(AB)

  AB_fix <- pipeline({
    fix
      fread
      select(which(names(.) %in% c('phase', names(AB))))
      filter(phase %in% names(AB[[1]]$val))
      mutate(w = unclass(X * (X > fine_th))[phase])
      gather(elm, wt, -phase, -w)
      mutate(
        i = pipeline({
          xmap[str_replace(elm, '[0-9]*O[0-9]*', '')]
            map(unlist, use.names = FALSE)
            map2_dbl(w, weightedMedian, na.rm = TRUE)
        }),
        val = wt / i,
        w = NULL, wt = NULL, i = NULL
      )
      nest(-elm, -phase)
      mutate(data = setNames(data, phase), phase = NULL)
      nest(-elm)
      mutate(data = setNames(data, elm), elm = NULL)
      `[[`('data')
      map(`[[`, 'data')
      map(map, unlist, use.names = FALSE)
      map(unlist)
  }) 
  
  for(e in names(AB_fix)) {
    for(p in names(AB_fix[[e]])){
      if(is.finite(AB_fix[[e]][[p]])) {
        AB[[e]][['val']][[p]][] <- AB_fix[[e]][[p]]
        AB[[e]][['se']][[p]][] <- 0
      }
    }
  }
  
  AB
}

