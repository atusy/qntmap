#' find AG
#' @noRd
#' @param epma A tidy epma data output by [`tidy_epma()`]
#' @param not_quantified
#'   A character vector specifying phases who weren't analyzed
#'   during point analysis # JAMSTEC
find_AG <- function(
                    epma,
                    not_quantified = character(0L) # © 2018 JAMSTEC
) {
  AG <- lm_AG(epma, .data$elm, .data$phase3)
  AG_mean <- lm_AG(epma, .data$elm)
  kept <- is.finite(AG[["a"]] * AG[["a_se"]])
  as.data.frame(bind_rows(
    AG[kept, ],
    left_join(
      AG[!kept, c("elm", "phase3", "g", "g_se")],
      AG_mean[, c("elm", "a", "a_se")],
      by = "elm"
    ),
    if (length(not_quantified) > 0L)
      unnest(mutate(AG_mean, phase3 = list(not_quantified)), "phase3")
  ))
}

#' @param epma `tidy_epma``
#' @param ... Grouping variables in NSE.
#' @importFrom stats coef lm sd vcov
#' @seealso [`find_B()`]
#' @noRd
lm_AG <- function(epma, ...) {
  mutate(
    ungroup(summarize(
      group_by(epma, ...),
      fit = list(lm(.data$wt ~ 0 + .data$net)),
      g = mean(.data$bgint),
      g_se = sd(.data$bgint) / (length(.data$bgint) - 1L)
    )),
    a = map(.data$fit, coef, complete = FALSE),
    a_se = map(.data$fit, vcov, complete = FALSE),
    len_eq_1 = (lengths(.data$a) * lengths(.data$a_se)) == 1L, # faster than x == 1L & y == 1L
    a = unlist(ifelse(.data$len_eq_1, .data$a, NA_real_), use.names = FALSE),
    a_se = unlist(ifelse(.data$len_eq_1, .data$a_se, NA_real_), use.names = FALSE),
    len_eq_1 = NULL,
    # a = map_dbl(fit, coef), a_se = map_dbl(fit, vcov),
    #  # Simpler codes works R 3.5.x
    fit = NULL
  )
}

# © 2018 JAMSTEC
#' Fix parameters: alpha, beta, and gamma
#' @noRd
#' @param params tidy parameters
#'
fix_AG <- function(params) {
  transmute(
    params, 
    elm = .data$oxide, phase3 = .data$phase, a = .data$alpha, g = .data$gamma
  )
}
