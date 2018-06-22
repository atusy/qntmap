#' find B
#' @param epma epma data
#' @importFrom dplyr filter
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
qntmap_B <- function(epma) {pipeline({
  epma 
    filter(!is.na(stg)) 
    group_by(elm) 
    mutate(
      fit_na = list(lm(pkint ~ 0 + map, weights = mem, na.action = na.omit))
    ) 
    group_by(stg, elm) 
    summarise(
      fit = list(lm(pkint ~ 0 + map, weights = mem)),
      fit_na = fit_na[1]
    ) 
    ungroup 
    mutate(
      b = map_dbl(fit, coef),
      fit = ifelse(is.na(b), fit_na, fit),
      b_se = map_dbl(fit, vcov),
      b = ifelse(is.na(b), map_dbl(fit_na, coef), b),
      fit = NULL,
      fit_na = NULL
    ) 
    nest(-stg, .key = '.B')
})}
