# © 2018 JAMSTEC
#' Pick values from mapping data
#'
#' Pick values from mapping data based on coordinates of x and y.
#'
#' @param .data
#'   Mapping data. Currentl version only supports `qm_xmap` object.
#' @param x,y
#'   Integer vectors specifying x- and y-coordinates of pixels of mapping data.
#'   In other words, x- and y- corresponds to n-th column and row of a matrix.
#' @param phase
#'   Name of phases comprising the pixels to be picked.
#'   If not manually supplied `phase` is supplied by `c('P1', 'P2', ...)`.
#' @param i
#'   A data frame with clumns `x`, `y`, and `phase`.
#'   `i` have priority to `x`, `y`, and `phase`.`
#' @param ...
#'   Ignored
#'
#' @return `data.frame`
#'
#' @seealso [add_centers()]
#'
#' @export
pick <- function(
                 .data,
                 x = NULL,
                 y = NULL,
                 phase = `if`(is.null(x), NULL, paste0("P", seq_along(x))),
                 i = data.frame(x, y, phase),
                 ...
) {
  if(length(unique(lengths(list(x, y, phase)))) != 1)
    stop("Lengths of x, y, and phase must be same")

  UseMethod("pick")
}

#' @rdname pick
#' @examples
#' # For qm_xmap class object
#' xm <- list(
#'   Si = as.data.frame(matrix(1:4, 2)),
#'   Ti = as.data.frame(matrix(5:8, 2))
#' )
#' class(xm) <- c("qm_xmap", "list")
#' 
#' ## following codes return same results
#' pick(xm, x = 1:2, y = 1:2)
#' pick(xm, x = 1:2, y = 1:2, phase = c("P1", "P2"))
#' pick(xm, i = data.frame(x = 1:2, y = 1:2, phase = c("P1", "P2")))
#' 
#' ## The last code is especially useful in case data frame is saved as a file such as csv.
#' \dontrun{
#' pick(xm, i = read.csv("pick_centers.csv"))
#' }
#' @export
pick.qm_xmap <- function() {
  data.frame(phase = i$phase, lapply(.data, `[`, cbind(i$y, i$x)))
}
formals(pick.qm_xmap) <- formals(pick)
