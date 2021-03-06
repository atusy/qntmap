#' Segment mapping area
#'
#' Make index to segment mapping area.
#' The obtained index is further utilized in [`mean.qntmap()`].
# © 2018 JAMSTEC

#'
#' @param x A path to the PNG image file.
#' @param ... ignored
#'
#' @return
#'   A character vector indicating pixel colors in RGBA style for input image.
#'
#' @seealso [`mean.qntmap()`]
#'
#' @export
segment <- function(x, ...) {
  if (!is.character(x) | length(x) != 1L)
    stop("x must be a path to the image file")
  UseMethod("segment")
}

#' @rdname segment
#'
#' @examples
#' x <- system.file("img", "Rlogo.png", package = "png")
#' head(segment(x))
#' @importFrom grDevices rgb
#' @importFrom png readPNG
#' @importFrom stats setNames
#'
#' @export
segment.character <- function(x, ...) {
  readPNG(x) %>>%
    apply(3L, list) %>>%
    setNames(names(formals(rgb))[seq_along(.)]) %>>%
    pmap(rgb) %>>%
    unlist(use.names = FALSE)
}
