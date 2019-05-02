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
#' class(qm) <- c("qntmap", "list")
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
#' mean(qm, index = rep(c("L", "R"), each = 100))
#' @importFrom dplyr group_by summarize
#' @importFrom tidyr gather spread
#'
#' @export
mean.qntmap <- function(x, index = "Whole area", ...) {
  x %>>%
    lapply(function(x) unlist(x[["wt"]], use.names = FALSE)) %>>%
    (~ if (length(index) %nin% c(1L, length(.[[1L]]))) {
      stop (
        "length of index must be 1 or same as number of pixels of qntmap:",
        length(.[[1L]])
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


#' Calculate means for horizontal and vertical directions
#'
#' Calculate means for horizontal and vertical directions for mapping data.
#' 
#' @param x 
#'   Mapping data (currently restricted to an object of class "qntmap").
#' @param step 
#'   A step size as a numeric value (i.e., length of pixel's sides).
#'   Specify when `x` is produced by `qntmap` before 0.3.4.
#' @export
hmean <- function(x) UseMethod("hmean")

hmean.data.frame <- function(x) Reduce(`+`, x) / ncol(x)

#' @export
hmean.qntmap <- function(x, step = attr(x, "step")[1L]) {
  x %>>%
    lapply(function(x) hmean.data.frame(x$wt)) %>>%
    post_hmean(step = step, new_class = "qm_hmean")
}

#' @rdname hmean
#' @export
vmean <- function(x) UseMethod("vmean") 

vmean.data.frame <- function(x) vapply(x, sum, 0) / nrow(x)

#' @export
vmean.qntmap <- function() {
  x %>>%
    lapply(function(x) vmean.data.frame(x$wt)) %>>%
    post_vmean(step = step, new_class = "qm_vmean")
}
formals(vmean.qntmap) <- formals(hmean.qntmap)

#' @noRd
#' @param x An object of class qm_hmean or qm_vmean
#' @param step A step size of mapping data
#' @param new_class An additional class, if any.
#' @param ... Arbitary attributes for a returning value.
#' 
#' @importFrom dplyr mutate row_number
post_hmean <- post_vmean <- function(x, step, new_class = NULL, ...) {
  x %>>%
    as.data.frame %>>%
    mutate(px = row_number() - 1L, um = px * !!step) %>>%
    prioritize(c('px', 'um')) %>>%
    structure(class = c(new_class, class(.)), step = step, ...)
}
