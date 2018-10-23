# small functions which are not exported

#' confidence interval of observed data from poisson process
#'
#' @param x data frame containing variable whose data is from poisson process
#' @param vars a character vector indicating which variables of x are poisson process data
#' @param offset vector, list, or data frame indicating rate of poisson process data.
#' @param conf.level confidence level for the returned confidence interval.
#'
#' @importFrom dplyr bind_cols
#' @importFrom pipeR pipeline
#' @importFrom purrr map
#' @importFrom purrr map2
#' @importFrom stats qgamma
#' @importFrom stats setNames
#' @noRd
cipois <- function(x, vars = names(x), offset = 1L, conf.level = 0.95) {
  low <- (1L - conf.level) / 2L
  high <- 1L - low

  pipeline({
    x[vars]
    map2(offset, `*`)
    map(round)
    map(
      function(x) {
        data.frame(
          L = qgamma(low, x),
          H = qgamma(high, x + 1L)
        )
      }
    )
    map2(offset, `/`)
    map(setNames, c('L', 'H'))
    unlist(recursive = FALSE)
    bind_cols(x)
  })
}

#' return integer as character flagged with 0
#'
#' @param ... list of integer vectors of same length
#' @noRd
flag0 <- function(...) {
  do.call(
    'paste0',
    lapply(
      unname(list(...)),
      function(x) {
        formatC(
          x,
          width = floor(log10(max(x, na.rm = TRUE))) + 1L, 
          flag = '0'
        )
      }
    )
  )
}

#' color LUT
#' @param palette palette to be used as LUT. either pcol (pseudocolor) or gray. It is ignored when LUT is specified.
#' @param n number of output colors. When n is more than number of colors in the specified palette, output contains duplicated colors.
#' @param dec FALSE in default outputs a vector of RGB colors. TRUE outputs matrix whose columns are R, G, and B, and whose values are in decimals.
#' @importFrom grDevices col2rgb
#' @noRd
mycolors <- function(palette = c('pcol', 'gray'), n = NULL, dec = FALSE) {
  LUT <- colors[[match.arg(palette)]]
  
  output <- `if`(
    is.null(n),
    LUT, # all colors
    LUT[seq(1L, length(LUT), length.out = n)] # n colors allowing duplicates
  )
  
  # dec = TRUE returns a matrix like R=0,...; G=255,...; B=129,....
  if(dec) return(`colnames<-`(t(col2rgb(output)), c('R', 'G', 'B')))

  # dec = FALSE returns a vector like #000000, #000001,...
  output
}

#' square
#' @param x input
#' @noRd
square <- function(x) x ^ 2L

#' Propagate add
#' @param x,y numeric
#' @param x2,y2 errors of x and y
#' @noRd
propagate_add <- function(x, x2, y, y2) {
  sqrt(square(x2 - x) + square(y2 - y))
}


#' Reduce add
#' @param x input
#' @noRd
reduce_add <- function(x) Reduce(`+`, x)

#' \%nin\%
#' @param x x
#' @param table table
#' @noRd
`%nin%` <- function(x, table) !match(x, table, nomatch = 0L)

#' Let certain components of x come prior to the alphabetically ordered others 
#' @param x must be named
#' @param prior priors
#' @noRd
prioritize <- function(x, prior) {
  nm <- names(x)
  x[c(
    intersect(prior, nm),
    sort(setdiff(nm, prior))
  )]
}
