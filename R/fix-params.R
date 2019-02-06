# © 2018 JAMSTEC
#' Fix parameters: alpha, beta, and gamma
#' @noRd
#' @param params tidy parameters
#' 
#' @importFrom dplyr distinct transmute left_join
NULL

fix_AG <- function (params) {
  transmute(params, elm = oxide, phase3 = phase, a = alpha, g = gamma)
}

fix_B <- function (params) {
  if(!is.null(params$stage))
    stop("Cannot inherit parameters from a file containing stage column.")
  
  distinct(transmute(params, elm = oxide, stg = "11", b = beta))
}
