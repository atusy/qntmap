#' color LUT
#' @param palette palette to be used as LUT. either pcol (= pseudocolor) or gray. It is ignored when LUT is specified.
#' @param n number of output colors. When n is more than number of colors in the specified palette, output contains duplicated colors.
#' @param dec FALSE in default outputs a vector of RGB colors. TRUE outputs matrix whose columns are R, G, and B, and whose values are in decimals.
#' @importFrom grDevices col2rgb
mycolors <- function(palette = c('pcol', 'gray'), n = NULL, dec = FALSE) {
    LUT <- colors[[match.arg(palette)]]

    if(is.null(n)) {
      #return all colors
      output <- LUT
    } else {
      #return n colors
      #n > length(LUT) is allowed by returning some duplicates
      output <- LUT[seq(1, length(LUT), length.out = n)]
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

