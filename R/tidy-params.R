# © 2018 JAMSTEC
#' Tidy parameters: alpha, beta, and gamma
#' @noRd
#' @param AG alpha and gamma returned by [`find_AG()`]
#' @param B beta returned by [`find_B()`]
#' 
#' @importFrom dplyr transmute left_join
tidy_params <- function (AG, B, qnt) {
  params <- left_join(AG, B, by = c("elm")) %>>%
    left_join(rename(qnt$elm[c("elem", "elint")], elm = elem), by = "elm") %>>%
    transmute(
      stage = stg, element = elm, elint, phase = phase3, 
      alpha = a, alpha_se = a_se,
      beta = b,  beta_se = b_se, 
      gamma = g, gamma_se = g_se
    ) 
  if(length(unique(B$stg)) == 1L) params$stage <- NULL
  params
}
