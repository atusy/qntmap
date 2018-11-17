#' find AG
#' @param epma A tidy epma data output by [`tidy_epma()`]
#' @param not_quantified 
#' A character vector specifying phases who weren't analyzed 
#' during point analysis # JAMSTEC
#' @importFrom dplyr group_by mutate summarise ungroup
#' @importFrom pipeR pipeline
#' @importFrom purrr map_dbl map
#' @importFrom stats sd lm coef vcov
#' @noRd
find_AG <- function(
  epma, 
  not_quantified = character(0) # © 2018 JAMSTEC
) {pipeline({
  epma
    group_by(elm)
    mutate(
      fit_na = list(lm(wt ~ 0 + net)),
      g_na = mean(bgint), # © 2018 JAMSTEC
      g_na_se = sd(g_na) / (length(g_na) - 1) # © 2018 JAMSTEC
    ) 
    group_by(elm, phase3) # © 2018 JAMSTEC changed group_by(phase3, elm) -> group_by(elm, phase3) 
    summarise(
      fit = list(lm(wt ~ 0 + net)),
      fit_na = fit_na[1],
      g = mean(bgint),
      g_se = sd(bgint) / (length(bgint) - 1),
      g_na = g_na[1], #  © 2018 JAMSTEC
      g_na_se = g_na_se[1] # © 2018 JAMSTEC
    ) 
    bind_rows(
      if(length(not_quantified)) {
        unnest(
          summarize(
            .,
            phase3 = list(not_quantified), # © 2018 JAMSTEC
            fit = fit_na[1], # © 2018 JAMSTEC
            g = g_na[1], # © 2018 JAMSTEC
            g_se = g_na_se[1] # © 2018 JAMSTEC
          ),
          phase3
        ) # © 2018 JAMSTEC
      } # © 2018 JAMSTEC
    ) # © 2018 JAMSTEC
    ungroup 
    mutate(
      g_na = NULL, g_na_se = NULL, # © 2018 JAMSTEC
      a = map_dbl(fit, coef),
      a_se = unlist(ifelse(is.na(a), map(fit_na, vcov), map(fit, vcov))),
      a = ifelse(is.na(a), map_dbl(fit_na, coef), a),
      ag = a * g,
      ag_se = sqrt((a * g_se) ^ 2 + (g * a_se) ^ 2),
      fit = NULL, fit_na = NULL, g = NULL, g_se = NULL
    )
})}
