#' find AG
#' @param epma epma data
#' @importFrom dplyr group_by
#' @importFrom dplyr mutate
#' @importFrom dplyr summarise
#' @importFrom dplyr ungroup
#' @importFrom pipeR pipeline
#' @importFrom purrr map_dbl
#' @importFrom purrr map
#' @importFrom stats sd
#' @importFrom stats lm 
#' @importFrom stats coef
#' @importFrom stats vcov
#' @noRd
find_AG <- function(epma) {pipeline({
  epma
    group_by(elm)
    mutate(fit_na = list(lm(wt ~ 0 + net))) 
    group_by(phase3, elm) 
    summarise(
      fit = list(lm(wt ~ 0 + net)),
      fit_na = fit_na[1],
      g = mean(bgint),
      g_se = sd(bgint) / (length(bgint) - 1)
    ) 
    ungroup 
    mutate(
      a = map_dbl(fit, coef),
      a_se = unlist(ifelse(is.na(a), map(fit_na, vcov), map(fit, vcov))),
      a = ifelse(is.na(a), map_dbl(fit_na, coef), a),
      ag = a * g,
      ag_se = sqrt((a * g_se) ^ 2 + (g * a_se) ^ 2),
      fit = NULL, fit_na = NULL, g = NULL, g_se = NULL
    )
})}
