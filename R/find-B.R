#' find B
#' @noRd
#' @param epma A tidy epma data output by [`tidy_epma()`]
find_B <- function(epma) {
  epma <- epma[
    !is.na(epma$stg),
    c("elm", "pkint", "mapint", "mem", "stg", "dwell", "beam_map")
  ]

  B <- lm_B(epma, .data$elm, .data$stg)
  kept <- is.finite(B$b + B$b_se)
  B <- bind_rows(
    B[kept, ],
    left_join(B[!kept, c("elm", "stg")], lm_B(epma, .data$elm), by = "elm")
  )

  as.data.frame(B)
}

#' Find beta (B) for small pieces of maps
#' @noRd
#'
#' @note
#' In case all weights are equal to `0`,
#' `lm` class object returns NA for `coef()` and `vcov()`
#'
#' `x <- 1:5; y <- rnorm(5) + x; w <- numeric(5); fit <- lm(y~0+x,weights=w); coef(fit); vcov(lm(y~0+x,weights=w))`
#'
#' For `coef()` and `vcov()`, `complete = FALSE` is used
#' for backward-compatibility (<R 3.4.x or before)
#'
#' @param epma `tidy_epma``
#' @param ... Grouping variables in NSE.
#' @importFrom stats coef lm vcov
lm_B <- function(epma, ...) {
  mutate(
    ungroup(summarize(
      group_by(epma, ...),
      fit = list(lm(.data$pkint ~ 0 + .data$mapint, weights = .data$mem)),
      k = .data$dwell[1L] * .data$beam_map[1L] * 1e+6
    )),
    b = map(.data$fit, coef, complete = FALSE),
    b_se = map(.data$fit, vcov, complete = FALSE),
    .kept = (lengths(.data$b) * lengths(.data$b_se)) == 1L,
    b = unlist(ifelse(.data$.kept, .data$b, NA_real_), use.names = FALSE) / .data$k,
    b_se = unlist(ifelse(.data$.kept, .data$b_se, NA_real_), use.names = FALSE) / .data$k,
    .kept = NULL,
    # b = vapply(fit, coef, 1.0) / k, b_se = vapply(fit, vcov, 1.0) / k,
    #  # Simple but works only after R 3.5.x
    fit = NULL, k = NULL
  )
}

# © 2018 JAMSTEC
#' Fix parameters: alpha, beta, and gamma
#' @noRd
#' @param params tidy parameters
#'
fix_B <- function(params) {
  if (!is.null(params$stage))
    stop("Cannot inherit parameters from a file containing stage column.")

  distinct(transmute(params, elm = .data$oxide, stg = "11", b = .data$beta))
}
