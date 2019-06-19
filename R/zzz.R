# Small internal functions

#' Confidence interval of observed data from poisson process
#'
#' @param x
#'   A data frame with variables following Poisson process
#' @param vars
#'   A character vector to specify columns of x following Poisson process.
#' @param offset
#'   A vector, a list, or a data frame indicating rate of poisson process data.
#' @param conf.level
#'   A confidence level for the returned confidence interval.
#'
#' @importFrom stats qgamma setNames
#' @noRd
cipois <- function(x, vars = names(x), offset = 1L, conf.level = 0.95) {
  low <- (1L - conf.level) / 2L
  high <- 1L - low

  x[vars] %>>%
    map2(offset, cipois_, low = low, high = high) %>>%
    unlist(recursive = FALSE) %>>%
    bind_cols(x)
}

#' Returns confidece interval of count data
#' @inheritParams cipois
#' @param low,high percentiles
#' @noRd
cipois_ <- function(x, offset = 1L, low = 0.025, high = 0.975) {
  x <- round(x * offset)
  data.frame(L = qgamma(low, x), H = qgamma(high, x + 1L)) / offset
}

#' return integer as character flagged with 0
#'
#' @param ... list of integer vectors of same length
#' @noRd
flag0 <- function(...) {
  do.call(
    "paste0",
    lapply(
      unname(list(...)),
      function(x) {
        formatC(
          x,
          width = floor(log10(max(x, na.rm = TRUE))) + 1L,
          flag = "0"
        ) # formatC
      } # function
    ) # lapply
  ) # do.call
}

#' Square
#' @param x input
#' @noRd
square <- function(x) x * x

#' L2 norm
#' @param x,y numeric
#' @noRd
L2 <- function(x, y) sqrt(square(x) + square(y))

#' Propagate add
#' @param x,y numeric
#' @param x2,y2 errors of x and y
#' @noRd
propagate_add <- function(x, x2, y, y2) L2(x2 - x, y2 - y)

#' Reduce add
#' @param x input
#' @noRd
reduce_add <- function(x) Reduce(`+`, x)

#' \%nin\%
#' @inheritParams match
#' @noRd
`%nin%` <- function(x, table) !match(x, table, nomatch = 0L)

#' Prioritize certain components of x, and order the others alphabetically
#' 
#' If the vector is unnamed, it is named by itself.
#' 
#' @param x A vector
#' @param ...
#'   Character vectors specifying the name of `x` which needs be prioritized.
#' @noRd
prioritize <- function(x, ...) {
  nm <- names(x)
  if (is.null(nm)) names(x) <- nm <- as.character(x)
  prior <- c(...)
  x[c(intersect(prior, nm), sort(setdiff(nm, prior)))]
}

#' Version of QntMap
#' @importFrom utils packageVersion
#' @noRd
ver <- getExportedValue("utils", "packageVersion")("qntmap")

#' Convert numeric to positive values
#' @noRd
as_positive <- function(x) x * (x > 0)
