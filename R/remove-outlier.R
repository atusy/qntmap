#' Remove outliers caused by multi-phase pixels
#'
#' When comparing data points of spot analysis and mapping analysis who share
#' the same coordinates, it is expected that they analyze the same phases.
#' However, this is unlikely when mapping analysis involves multi-phase pixels
#' due to sizes of pixels and phases. This function removes such multi-phase
#' pixels by finding outliers of mapping peak intensities.
#'
#' @param epma A value returned by `tidy_epma()`
#' @param interval
#'  A type of the interval. Data points outside intervals are treated as outliers.
#'  If `"prediction"`` (default), prediction intervals are used based on Poisson process.
#'  If `"tukey"``, conditional lower and upper whiskers are used where the lower is 
#'  $Q_{1/4} - 1.5 IQR$ and the upper is $Q_{3/4} + 1.5 IQR$.
#' @param method
#'  Applicable when `interval = "prediction"`.
#'  If `"rq"`` (default), quantile regression is performed.
#'  If `"lsfit"`, least square regression is performed.
#' @param percentile
#'  A percentile of predictive interaval.
#'  Applicable when `interval = "prediction"`.
#' 
#' @export
remove_outlier <- function(
  epma, interval = c("prediction", "tukey"), method = c("rq", "lsft", "median"), percentile = .99
) {
  method <- match.arg(method)
  
  find_intervals <- list(
    tukey = find_whiskers,
    prediction = find_poisson_prediction_intervals
  )[[match.arg(interval)]]
  
  epma %>>%
    select(
      "beam", "pk_t", "dwell", "beam_map", "map",
      .data$id, .data$phase, .data$elint,
      starts_with("mapint"), starts_with("pkint")
    ) %>>%
    filter(is.finite(.data$mapint), is.finite(.data$pkint)) %>>%
    mutate(n_elint = length(unique(.data$elint))) %>>%
    find_intervals(method = method, percentile = percentile) %>>%
    filter(
      (.data$pkint.L_est < .data$pkint.H & .data$pkint.L < .data$pkint.H_est) | 
        (.data$mapint.L_est < .data$mapint.H & .data$mapint.L < .data$mapint.H_est)
    ) %>>%
    group_by(.data$id) %>>%
    filter(.data$n_elint == n()) %>>%
    ungroup %>>%
    mutate(n_elint = NULL) %>>%
    as.data.frame %>>%
    structure(class = c("qm_epma_filtered", class(.)))
}


#' @importFrom quantreg rq
find_whiskers <- function(x, ...) {
  x %>>%
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
    ungroup() %>>%
    mutate(
      iqr = .data$q75 - .data$q25, 
      k_lo = .data$q25 - 1.5 * .data$iqr,
      k_hi = .data$q75 + 1.5 * .data$iqr,
      iqr = NULL, q25 = NULL, q75 = NULL,
      pkint.L_est = .data$mapint * .data$k_lo,
      pkint.H_est = .data$mapint * .data$k_hi,
      mapint.L_est = .data$pkint / .data$k_hi,
      mapint.H_est = .data$pkint / .data$k_lo,
      # k_lo = NULL, k_hi = NULL
    )
}

#' @importFrom quantreg rq
#' @importFrom stats lsfit qnbinom median
find_poisson_prediction_intervals <- function(
  x, percentile = .99, method = c("rq", "lsfit", "median")
) {
  reg <- list(
    lsfit = function(x, y) stats::lsfit(x, y, intercept = FALSE)$coefficients,
    rq = function(x, y) quantreg::rq(y ~ 0 + x, tau = .5)$coefficients,
    median = function(x, y) stats::median((y / x)[x != 0])
  )[[match.arg(method)]]
  
  lo = (1 - percentile) / 2
  hi = 1 - lo
  
  x %>>%
    group_by(.data$elint) %>>%
    mutate(k = reg(.data$mapint, .data$pkint)) %>>%
    ungroup() %>>%
    mutate(
      temp = .data$pk_t * .data$beam * 1e6,
      pkint.H_est = qnbinom(!!hi, .data$mapint.H * .data$k * .data$temp, .5) / .data$temp,
      pkint.L_est = qnbinom(!!lo, .data$mapint.L * .data$k * .data$temp, .5) / .data$temp,
      temp = .data$dwell * .data$beam_map * 1e6,
      mapint.L_est = qnbinom(!!hi, .data$pkint.H / .data$k * .data$temp, .5) / .data$temp,
      mapint.H_est = qnbinom(!!lo, .data$pkint.L / .data$k * .data$temp, .5) / .data$temp,
      temp = NULL, k = NULL
    )
}
