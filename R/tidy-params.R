# © 2018 JAMSTEC
#' Tidy parameters: alpha, beta, and gamma
#' @noRd
#' @param AG alpha and gamma returned by [`find_AG()`]
#' @param B beta returned by [`find_B()`]
#'
tidy_params <- function(AG, B, qnt) {
  params <- left_join(AG, B, by = c("elm")) %>>%
    left_join(rename(qnt$elm[c("elem", "elint")], elm = "elem"), by = "elm") %>>%
    transmute(
      stage = .data$stg, oxide = .data$elm, element = .data$elint, phase = .data$phase3,
      alpha = .data$a, beta = .data$b, gamma = .data$g, wt = NA_real_
    )
  if (length(unique(B$stg)) == 1L) params$stage <- NULL
  params
}
