#' Filter multi-phase pixels based on Tukey's choice of outliers
#'
#' By quantile regression, conditional lower and upper whiskers are calculated for pkint and mapint. Then, keep data points whose 95% confidence intervals overlaps with the range of conditional whiskers. If any element belonging to a particular id is filtered, then that id is also filtered.
#'
#' @param epma A value returned by `tidy_epma()`
#' 
#' @importFrom quantreg rq
#' 
#' @noRd
filter_outlier <- function(epma) {
  epma %>>%
    select(
      .data$id, .data$phase, .data$elint,
      starts_with("mapint"), starts_with("pkint")
    ) %>>%
    mutate(n_elint = length(unique(.data$elint))) %>>%
    group_by(.data$elint) %>>%
    group_modify(~ {
      mutate(
        .x,
        !!!setNames(
          c(rq(pkint ~ 0 + mapint, tau = c(.25, .75), data = .x)$coefficients),
          c("q25", "q75")
        )
      )
    }) %>>%
    ungroup %>>%
    mutate(
      iqr = .data$q75 - .data$q25, 
      whisker_lo = .data$q25 - 1.5 * .data$iqr,
      whisker_up = .data$q75 + 1.5 * .data$iqr,
      iqr = NULL, q25 = NULL, q75 = NULL,
      pkint.L_est = .data$mapint * .data$whisker_lo,
      pkint.H_est = .data$mapint * .data$whisker_up,
      mapint.L_est = .data$pkint / .data$whisker_up,
      mapint.H_est = .data$pkint / .data$whisker_lo,
      whisker_lo = NULL, wisker_hi = NULL
    ) %>>%
    filter(
      (.data$pkint.L_est < .data$pkint.H & .data$pkint.L < .data$pkint.H_est) | 
        (.data$mapint.L_est < .data$mapint.H & .data$mapint.L < .data$mapint.H_est)
    ) %>>%
    group_by(.data$id) %>>%
    filter(.data$n_elint == n()) %>>%
    ungroup %>>%
    mutate(n_elint = NULL) %>>%
    as.data.frame %>>%
    structure(class = c("qm_epma_filtered", "qm_epma", class(.)))
}
