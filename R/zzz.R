# small functions which are not exported

#' confidence interval of observed data from poisson process
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
#' @importFrom dplyr bind_cols
#' @importFrom purrr map map2
#' @importFrom stats qgamma setNames
#' @noRd
cipois <- function(x, vars = names(x), offset = 1L, conf.level = 0.95) {
  low <- (1L - conf.level) / 2L
  high <- 1L - low

  x[vars] %>>%
    map2(offset, `*`) %>>%
    map(round) %>>%
    map(
      function(x) data.frame(L = qgamma(low, x), H = qgamma(high, x + 1L))
    ) %>>%
    map2(offset, `/`) %>>%
    map(setNames, c('L', 'H')) %>>%
    unlist(recursive = FALSE) %>>%
    bind_cols(x)
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
      function (x) {
        formatC(
          x,
          width = floor(log10(max(x, na.rm = TRUE))) + 1L, 
          flag = '0'
        ) # formatC
      } # function
    ) # lapply
  ) # do.call
}

#' color LUT
#' @param palette 
#'   A palette to be used as LUT: pcol (pseudocolor) or gray.
#' @param n 
#'   A number of required colors. 
#'   A large `n` may return duplicates.
#' @param dec 
#'   `FALSE` (default) outputs a character vector in `#ffffff` format. 
#'   `TRUE` outputs matrix with columns are R, G, and B, 
#'   whose values are in decimals.
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

#' Square
#' @param x input
#' @noRd
square <- function(x) x ^ 2L

#' L2 norm
#' @param x,y numeric
#' @noRd
L2 <- function(x, y) sqrt(square(x) + square(y))

#' Propagate add
#' @param x,y numeric
#' @param x2,y2 errors of x and y
#' @noRd
propagate_add <- function(x, x2, y, y2) {
  L2(x2 - x, y2 - y)
}


#' Reduce add
#' @param x input
#' @noRd
reduce_add <- function(x) Reduce(`+`, x)

#' \%nin\%
#' @inheritParams match
#' @noRd
`%nin%` <- function(x, table) !match(x, table, nomatch = 0L)

#' Prioritize certain components of x, and order the others alphabetically
#' @param x A named object
#' @param prior 
#'   A character vector specifying the name of `x` which needs be prioritized.
#' @noRd
prioritize <- function(x, prior) {
  nm <- names(x)
  x[c(intersect(prior, nm), sort(setdiff(nm, prior)))]
}

#' Version of QntMap
#' @importFrom utils packageVersion
#' @noRd
ver <- getExportedValue('utils', 'packageVersion')('qntmap')