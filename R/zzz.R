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


#' return integer as character flagged with 0
#'
#' @param ... list of integer vectors of same length
flag0 <- function(...) {
  do.call(
    'paste0',
    lapply(
      unname(list(...)),
      function(x) {
        formatC(
          x,
          width = floor(log10(max(x, na.rm = TRUE))) + 1, 
          flag = '0'
        )
      }
    )
  )
}


#' color LUT
#' @param palette palette to be used as LUT. either pcol (= pseudocolor) or gray. It is ignored when LUT is specified.
#' @param n number of output colors. When n is more than number of colors in the specified palette, output contains duplicated colors.
#' @param dec FALSE in default outputs a vector of RGB colors. TRUE outputs matrix whose columns are R, G, and B, and whose values are in decimals.
#' @importFrom grDevices col2rgb
mycolors <- function(palette = c('pcol', 'gray'), n = NULL, dec = FALSE) {
  LUT <- colors[[match.arg(palette)]]
  
  output <- 
    if(is.null(n)) {
      #return all colors
      LUT
    } else {
      #return n colors
      #n > length(LUT) is allowed by returning some duplicates
      LUT[seq(1, length(LUT), length.out = n)]
    }
  
  if(dec){
    #TRUE returns a matrix like R=0,...; G=255,...; B=129,....
    #FALSE returns a vector like #000000, #000001,...
    output <- t(col2rgb(output))
    colnames(output) <- c('R', 'G', 'B')
  }
  
  #return available LUTs or colors chosen from a LUT
  output
}



#' Reduce add
#' @param x input
reduce_add <- function(x) Reduce(`+`, x)

