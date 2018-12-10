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
      fit = list(lm(pkint ~ 0 + mapint, weights = mem)),
      k = dwell[1] * beam_map[1] * 1e+6
    )),
    b = map(fit, coef, complete = FALSE),
    b_se = map(fit, vcov, complete = FALSE),
    .kept = map_int(b, length) == 1 & map_int(b_se, length) == 1,
    b = unlist(ifelse(.kept, b, NA_real_), use.names = FALSE) / k,
    b_se = unlist(ifelse(.kept, b_se, NA_real_), use.names = FALSE) / k,
    .kept = NULL,
    # b = map_dbl(fit, coef) / k, b_se = map_dbl(fit, vcov) / k, # Simpler codes works R 3.5.x
    fit = NULL, k = NULL
  )
}
