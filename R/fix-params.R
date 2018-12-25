# © 2018 JAMSTEC
#' Fix parameters: alpha, beta, and gamma
#' @noRd
#' @param params tidy parameters
#' 
#' @importFrom dplyr distinct transmute left_join
NULL

fix_AG <- function (params) {
  transmute(params,
    elm = element, phase3 = phase, 
    a = alpha, a_se = alpha_se, g = gamma, g_se = gamma_se
  )
}

fix_B <- function (params) {
  if(!is.null(params$stage))
    stop("Cannot inherit parameters from a file containing stage column.")
  
  distinct(transmute(params,
    elm = element, stg = "11", b = beta, b_se = beta_se
  ))
}
