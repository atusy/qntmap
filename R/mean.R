# © 2018 JAMSTEC
#' @name mean
#' @title Arithmetric mean for `qntmap` package
#' @description 
#'   S3 methods for the arithmetric mean. 
#'   See [base::mean()]  for general use of `mean()`.
#' 
#' @param x 
#'   A `qntmap` class object returned by [quantify()] or [qntmap()].
#' @param index 
#'   A vector of length `1`` or length equal to number of pixels in map. 
#'   An index can be created using mask image through [segment()].
#' @param ... 
#'   Other arguments passed to [base::mean.default()]
#' 
#' @section mean.qntmap:
#'   A returning value is a [`data.frame`] whose first column lists elements,
#'   and second to last columns lists `mean` values of each elements by index.
#' 
#' @seealso [base::mean()], [segment()], [quantify()], [qntmap()]
NULL

#' @rdname mean
#' 
#' @examples 
#' # Calculate mean for qntmap class object
#' 
#' ## mapping data corresponds of 100 x 2 pixels
#' ## element A in the left half of the map yields 
#' ## average 50 wt% with standard deviation = 1.
#' ## and in the right half of the map yields 
#' ## average 50 wt% with standard deviation = 1.
#' qm <- list(
#'   A = list(wt = data.frame(rnorm(100, 50), rnorm(100, 20)))
#' )
#' class(qm) <- c('qntmap', 'list')
#' 
#' ## If index is not specified, 
#' ## mean value is calculated from whole area,
#' ## which in this case gives a value around 35.
#' mean(qm)
#' 
#' ## If index is given, 
#' ## mean is calculated from the pixels sharing the same index.
#' ## For example, pixels in the left-half of the map is labelled by 'L',
#' ## and the other side by 'R'.
#' ## In such a case, mean of A is around 50 fo 'L' and 30 for 'R'
#' mean(qm, index = rep(c('L', 'R'), each = 100))
#' 
#' @importFrom dplyr group_by summarize
#' @importFrom tidyr gather spread
#' 
#' @export
mean.qntmap <- function (x, index = 'Whole area', ...) {
    x %>>%
    lapply(`[[`, 'wt') %>>%
    lapply(unlist, use.names = FALSE) %>>%
    (~ if (length(index) %nin% c(1, length(.[[1]]))) {
      stop (
        'length of index must be 1 or same as number of pixels of qntmap:', 
        length(.[[1]])
      )
    }) %>>%
    c(.index = list(index)) %>>%
    as.data.frame() %>>%
    gather(Element, val, -.index, factor_key = TRUE) %>>%
    group_by(Element, .index) %>>%
    summarize(val = mean.default(val, ...)) %>>%
    spread(.index, val) %>>%
    as.data.frame()
}
