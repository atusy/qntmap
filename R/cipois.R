#' confidence interval of observed data from poisson process
#'
#' @param x data frame containing variable whose data is from poisson process
#' @param vars a character vector indicating which variables of x are poisson process data
#' @param offset vector, list, or data frame indicating rate of poisson process data.
#' @param conf.level confidence level for the returned confidence interval.
#'
#' @importFrom dplyr bind_cols
#' @importFrom purrr map
#' @importFrom purrr map2
#' @importFrom stats qgamma
#' @importFrom stats setNames
#'
#' @export
#'
cipois <- function(x, vars = names(x), offset = 1, conf.level = 0.95) {
  low <- (1 - conf.level) / 2
  high <- 1 - low
  
  x[vars] %>>%
    map2(offset, `*`) %>>%
    map(round) %>>%
    map(
      function(x) {
        data.frame(
          L = qgamma(low, x),
          H = qgamma(high, x + 1)
        )
      }
    ) %>>%
    map2(offset, `/`) %>>%
    map(setNames, c('L', 'H')) %>>%
    unlist(recursive = FALSE) %>>%
    bind_cols(x)
}
