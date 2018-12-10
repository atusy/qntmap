#' find B
#' @param epma A tidy epma data output by [`tidy_epma()`]
#' @param fix fix B
#' @importFrom dplyr bind_rows left_join
#' @noRd
find_B <- function(epma, fix = NULL) {
  epma <- epma[
    !is.na(epma$stg),
    c("elm", "pkint", "mapint", "mem", "stg", "dwell", "beam_map")
  ]

  B <- lm_B(epma, elm, stg)
  kept <- is.finite(B$b + B$b_se)
  B <- bind_rows(
    B[kept, ],
    left_join(B[!kept, c("elm", "stg")], lm_B(epma, elm), by = "elm")
  )
  
  B[B$elm %in% fix, c("b", "b_se")] <- list(1, NA_real_)
  
  as.data.frame(B)
}

#' find beta (B) for small pieces of maps
#' 
#' @note
#' In case all weights are equal to `0`, 
#' `lm` class object returns NA for `coef()` and `vcov()`
#' `x <- 1:5; y <- rnorm(5) + x; w <- numeric(5); fit <- lm(y~0+x,weights=w); coef(fit); vcov(lm(y~0+x,weights=w))`
#' 
#' For `coef()` and `vcov()`, `complete = FALSE` is used
#' for backward-compatibility (<R 3.4.x or before)
#' 
#' @param epma `tidy_epma``
#' @param ... Grouping variables in NSE.
#' @importFrom dplyr group_by mutate summarize ungroup
#' @importFrom purrr map_dbl
#' @importFrom stats coef lm vcov
#' @noRd
lm_B <- function(epma, ...) {
  mutate(
    ungroup(summarize(
      group_by(epma, ...), 
      zeros = all(mem == 0),
      fit = list(lm(pkint ~ 0 + mapint, weights = mem)),
      k = dwell[1] * beam_map[1] * 1e+6
    )),
    b = unlist(ifelse(zeros, NA_real_, map(fit, coef)), use.names = FALSE),
    b_se = unlist(ifelse(zeros, NA_real_, map(fit, vcov)), use.names = FALSE),
    zeros = NULL,
    # map_dbl(fit, coef) / k, b_se = map_dbl(fit, vcov) / k, # Simpler codes works R 3.5.x
    fit = NULL, k = NULL
  )
}
