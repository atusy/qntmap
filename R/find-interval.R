#' Candidates of intervals to estimate
#' 
#' @param x Numeric vectors
#' 
#' @noRd
interval_type <- list(
  credible = list(
    L = function(x, lo, ...) qgamma(lo, x),
    H = function(x, hi, ...) qgamma(hi, x)
  ),
  predictive = list(
    L = function(x, lo, ...) qnbinom(lo, x, .5),
    H = function(x, hi, ...) qnbinom(hi, x, .5)
  )
)

#' Find intervals on selected variables
#' 
#' Adds the result to new columns suffixed by `.L` and `.H`
#' 
#' @param x data frame
#' @param type Type of intervals.
#' @param range Range of intervals.
#' @inheritParams tidyselect::vars_select
#' 
#' @noRd
find_interval <- function(
  x, ..., type = c("predictive", "credible"), range = 0.95, .strict = FALSE
) {
  .vars <- vars_select(names(x), ..., .strict = .strict)
  type = match.arg(type)
  lo = (1 - range) / 2
  hi = 1 - lo
  x %>>%
    mutate_at(.vars, interval_type[[type]], lo = lo, hi = hi) %>%
    rename_at(vars(matches("^[LH]$")), function(x) paste0(.vars, "_", x)) %>%
    rename_at(
      vars(matches("_[LH]$")), str_replace_all, c("_L$" = ".L", "_H$" = ".H")
    )
}
