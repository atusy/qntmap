#' Find outliers caused by multi-phase pixels
#'
#' This function add to the data frame generated by `tidy_epma()` columns
#' `outlier` (boolean), `pkint.L_est`, `pkint.H_est`, `mapint.L_est`, `beta`, and
#' `mapint.H_est`. Required columns in input are `id`, `elint`, `phase`, `pk_t`, `beam`, `dwell`, `beam_map`, `mapint`, `mapint.L`, `mapint.H`, `pkint`, `pkint.L`, `pkint.H`.
#' 
#' When comparing data points of spot analysis and mapping analysis who share
#' the same coordinates, it is expected that they analyze the same phases.
#' However, this is unlikely when mapping analysis involves multi-phase pixels
#' due to sizes of pixels and phases. This function removes such multi-phase
#' pixels by finding outliers of mapping peak intensities.
#'
#' @param epma A value returned by [tidy_epma()]
#' @param phase,element
#'  Selected ones are referenced to detect outliers. Default selects everything.
#'  Tidy selection is available. For example `c(Si, Ti)` selects them, and
#'  `c(-Si, -Ti)` selects everything except them.
#' @param interval
#'  A type of the interval. Data points outside intervals are treated as outliers.
#'  If `"prediction"` (default), prediction intervals are used based on Poisson process.
#'  If `"tukey"``, conditional lower and upper whiskers are used where the lower is 
#'  $Q_{1/4} - 1.5 IQR$ and the upper is $Q_{3/4} + 1.5 IQR$.
#' @param method
#'  Applicable when `interval = "prediction"`.
#'  If `"rq"` (default), quantile regression is performed ([quantreg::rq()]).
#'  If `"lsfit"`, least square regression is performed ([stats::lsfit()]).
#' @param percentile
#'  A percentile of predictive interaval.
#'  Applicable when `interval = "prediction"`.
#' @param fine_phase
#'  Deprecated as of qntmap > 0.4.0. Use `phase` instead.
#' 
#' @seealso [quantreg::rq()], [stats::lsfit()], [tidyselect::vars_select()], [tidy_epma()]
#' 
#' @export
find_outlier <- function(
  epma,
  phase = everything(),
  element = everything(),
  interval = c("prediction", "tukey"),
  method = c("rq", "lsfit", "median"),
  percentile = .99,
  fine_phase = NULL
) {
  deprecate_fine_phase(fine_phase)
  
  method <- match.arg(method)
  
  find_intervals <- list(
    tukey = find_whiskers,
    prediction = find_poisson_prediction_intervals
  )[[match.arg(interval)]]
  
  .element <- vars_select(unique(epma$elint), !!enquo(element))
  .phase <- setdiff(vars_select(unique(epma$phase), !!enquo(phase)), fine_phase)
  
  epma %>>%
    find_intervals(phase = .phase, method = method, percentile = percentile) %>>%
    group_by(.data$id) %>>%
    mutate(
      outlier = any(
        (.data$pkint.H < .data$pkint.L_est | .data$pkint.H_est < .data$pkint.L) &
          (.data$mapint.H < .data$mapint.L_est | .data$mapint.H_est < .data$mapint.L) &
          (.data$elint %in% !!.element) |
          (.data$phase %nin% !!.phase)
      )
    ) %>>%
    ungroup %>>%
    as.data.frame %>>%
    structure(class = c("qm_epma_filtered", class(.)))
}


#' @importFrom stats na.omit
#' @importFrom quantreg rq
#' @param phase character vector
#' @noRd
find_whiskers <- function(x, phase = NULL, ...) {
  x %>>%
    mutate(
      .pkint = .data$pkint * c(NA_real_, 1)[1L + (.data$phase %in% !!phase)]
    ) %>>%
    group_by(.data$elint) %>>%
    group_modify(~ {
      mutate(
        .x,
        !!!setNames(
          rep_len(
            c(rq(
              .pkint ~ 0 + mapint, tau = c(.25, .50, .75),
              data = .x, na.action = na.omit
            )$coefficients),
            3L
          ),
          c("q25", "beta", "q75")
        )
      )
    }) %>>%
    ungroup() %>>%
    mutate(
      .pkint = NULL,
      iqr = .data$q75 - .data$q25, 
      k_lo = .data$q25 - 1.5 * .data$iqr,
      k_hi = .data$q75 + 1.5 * .data$iqr,
      iqr = NULL, q25 = NULL, q75 = NULL,
      pkint.L_est = .data$mapint * .data$k_lo,
      pkint.H_est = .data$mapint * .data$k_hi,
      mapint.L_est = .data$pkint / .data$k_hi,
      mapint.H_est = .data$pkint / .data$k_lo,
      k_lo = NULL, k_hi = NULL
    )
}

#' @importFrom quantreg rq
#' @importFrom stats lm qnbinom median
#' @param phase character vector
#' @noRd
find_poisson_prediction_intervals <- function(
  x, phase = NULL, percentile = .99, method = c("rq", "lsfit", "median"), ...
) {
  method <- match.arg(method)
  lo = (1 - percentile) / 2
  hi = 1 - lo
  
  x %>>%
    mutate(
      .pkint = .data$pkint * c(NA_real_, 1)[1L + (.data$phase %in% !!phase)]
    ) %>>%
    group_by(.data$elint) %>>%
    mutate(beta = fit(method, .data$mapint, .data$.pkint)) %>>%
    ungroup() %>>%
    mutate(
      .pkint = NULL,
      temp = .data$pk_t * .data$beam * 1e6,
      pkint.H_est =
        qnbinom(!!hi, .data$mapint.H * .data$beta * .data$temp, .5) / .data$temp,
      pkint.L_est =
        qnbinom(!!lo, .data$mapint.L * .data$beta * .data$temp, .5) / .data$temp,
      temp = .data$dwell * .data$beam_map * 1e6,
      mapint.L_est =
        qnbinom(!!hi, .data$pkint.H / .data$beta * .data$temp, .5) / .data$temp,
      mapint.H_est =
        qnbinom(!!lo, .data$pkint.L / .data$beta * .data$temp, .5) / .data$temp,
      temp = NULL, k = NULL
    )
}

fit <- function(method, x, y, tau = .5) {
  if (all(!is.finite(x * y))) return(NA_real_)
  list(
    lsfit = function(x, y) {
      stats::lm(y ~ 0 + x, intercept = FALSE, na.action = na.omit)$coefficients
    },
    rq = function(x, y) {
      quantreg::rq(y ~ 0 + x, tau = tau, na.action = na.omit)$coefficients
    },
    median = function(x, y) {
      stats::median((y / x)[x != 0], na.rm = TRUE)
    }
  )[[method]](x, y)
}
