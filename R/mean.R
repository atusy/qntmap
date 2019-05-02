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
#' @param density
#'   A named numeric vector specifying density of all the phases in `x`.
#'   (e.g., `c(Qtz = 2.6, Ab = 2.6)`).
#' @param cluster
#'   A result of `cluster_xmap()` (i.e., `qm_cluster` class object).
#'   `cluster` is required when specifying `density`.
#' @param ...
#'   Ignored
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
mean.qntmap <- function(x, index = "Whole area", cluster = NULL, density = NULL, ...) {
  # Provide densities
  density <- if (is.null(cluster)) {
      1 
    } else if (!all(cluster$cluster %in% names(density))) {
      stop("`density` must be a named vector to specify density of all the phases")
    } else {
      density[cluster$cluster]
    }
  
  x %>>%
    lapply(function(x) unlist(x[["wt"]], use.names = FALSE)) %>>%
    (~ if (length(index) %nin% c(1L, length(.[[1L]]))) {
      stop(
        "length of index must be 1 or same as number of pixels of qntmap:",
        length(.[[1L]])
      )
    }) %>>%
    as.data.frame() %>>%
    mutate(.index = !!index, .density = !!density) %>>%
    gather(Element, val, -.index, -.density, factor_key = TRUE) %>>%
    mutate(val = val * .density) %>>%
    group_by(Element, .index) %>>%
    summarize(val = sum(val), .density = sum(.density)) %>>%
    ungroup() %>>%
    mutate(val = val / .density, .density = NULL) %>>%
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
#' @param ...
#'   Arguments passed to `mean.qntmap()` 
#'   if x is a `qntmap` class object returned by `quantify()` or `qntmap()`.
#'   Otherwise ignored.
#' @seealso mean.qntmap
#' 
#' @export
hmean <- function(x, step = attr(x, "step")[1L], ...) UseMethod("hmean")

#' @export
hmean.qntmap <- function() {
  x %>>%
    mean.qntmap(
      index = rep(seq(0L, nrow(x[[1]]$wt) - 1L), ncol(x[[1]]$wt)), ...
    ) %>>%
    as_hmean(step = step, new_class = "qm_hmean")
}
formals(hmean.qntmap) <- formals(hmean)

#' @rdname hmean
#' @export
vmean <- function() UseMethod("vmean") 
formals(vmean) <- formals(hmean)

#' @export
vmean.qntmap <- function() {
  x %>>%
    mean.qntmap(
      index = rep(seq(0L, ncol(x[[1]]$wt) - 1L), each = nrow(x[[1]]$wt)), ...
    ) %>>%
    as_vmean(step = step, new_class = "qm_vmean")
}
formals(vmean.qntmap) <- formals(hmean)

#' @noRd
#' @param x An object of class qm_hmean or qm_vmean
#' @param step A step size of mapping data
#' @param new_class An additional class, if any.
#' @param ... Arbitary attributes for a returning value.
#' 
#' @importFrom dplyr mutate row_number
as_hmean <- as_vmean <- function(x, step, new_class = NULL, ...) {
  x %>>%
    gather(px, wt, -Element) %>>%
    spread(Element, wt) %>>%
    mutate(px = row_number() - 1L, um = px * !!step) %>>%
    prioritize(c('px', 'um')) %>>%
    structure(class = c(new_class, class(.)), step = step, ...)
}
