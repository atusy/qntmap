#' find B
#' @param epma epma data
#' @param fix fix B
#' @importFrom dplyr group_by
#' @importFrom dplyr mutate
#' @importFrom dplyr summarise
#' @importFrom dplyr ungroup
#' @importFrom tidyr nest
#' @importFrom stats lm
#' @importFrom stats na.omit
#' @importFrom stats vcov
#' @importFrom stats coef
#' @importFrom purrr map_dbl
#' @importFrom pipeR pipeline 
#' 
find_B <- function(epma, fix = NULL) {pipeline({
  epma[!is.na(epma$stg), ] 
    group_by(elm) 
    mutate(
      fit_na = list(lm(pkint ~ 0 + mapint, weights = mem, na.action = na.omit))
    ) 
    group_by(stg, elm) 
    summarise(
      fit = list(lm(pkint ~ 0 + mapint, weights = mem)),
      fit_na = fit_na[1],
      k = dwell[1] * beam_map[1] * 1e+6
    ) 
    ungroup 
    mutate(
      fix = elm %in% fix,
      b = map_dbl(fit, coef),
      fit = ifelse(is.na(b), fit_na, fit),
      b_se = ifelse(fix, 0, map_dbl(fit, vcov) / k),
      b = ifelse(fix, 1, ifelse(is.na(b), map_dbl(fit_na, coef), b)) / k,
      fit = NULL,
      fit_na = NULL,
      k = NULL,
      fix = NULL
    ) 
    nest(-stg, .key = '.B')
})}

