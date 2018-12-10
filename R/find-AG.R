#' find AG
#' @param epma A tidy epma data output by [`tidy_epma()`]
#' @param not_quantified 
#' A character vector specifying phases who weren't analyzed 
#' during point analysis # JAMSTEC
#' @importFrom dplyr group_by mutate summarise ungroup
#' @importFrom purrr map_dbl map
#' @importFrom stats sd lm coef vcov
#' @noRd
find_AG <- function(
  epma, 
  not_quantified = character(0) # © 2018 JAMSTEC
) {
  AG <- lm_AG(epma, elm, phase3)
  AG_mean <- lm_AG(epma, elm)
  kept <- is.finite(AG[["a"]] *AG[["a_se"]])
  as.data.frame(bind_rows(
    AG[kept, ],
    left_join(
      AG[!kept, c("elm", "phase3", "g", "g_se")], 
      AG_mean[, c("elm", "a", "a_se")], 
      by = "elm"
    ),
    if(length(not_quantified) > 0)
      unnest(mutate(AG_mean, phase3 = not_quantified))
  ))
}

#' @param epma `tidy_epma``
#' @param ... Grouping variables in NSE.
#' @importFrom dplyr group_by mutate summarize ungroup
#' @importFrom purrr map_dbl
#' @importFrom stats coef lm vcov
#' @seealso [`find_B()`]
#' @noRd
lm_AG <- function(epma, ...) {
  mutate(
    ungroup(summarize(
      group_by(epma, ...), 
      fit = list(lm(wt ~ 0 + net)),
      g = mean(bgint),
      g_se = sd(bgint) / (length(bgint) - 1)
    )),
    a = map(fit, coef, complete = FALSE),
    a_se = map(fit, vcov, complete = FALSE),
    len_eq_1 = lengths(a) == 1 & lengths(a_se) == 1,
    a = unlist(ifelse(len_eq_1, a, NA_real_), use.names = FALSE),
    a_se = unlist(ifelse(len_eq_1, a_se, NA_real_), use.names = FALSE),
    len_eq_1 = NULL,
    # a = map_dbl(fit, coef), a_se = map_dbl(fit, vcov), # Simpler codes works R 3.5.x
    fit = NULL
  )
}
