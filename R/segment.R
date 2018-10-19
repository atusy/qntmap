#' Segment mapping area
#' 
#' Make index to segment mapping area.
#' The obtained index is further utilized in [mean.qntmap()].
# © 2018 JAMSTEC

#' 
#' @param x A path to the image file (PNG, JPEG, and BMP).
#' @param ... ignored
#' 
#' @return A character vector indicating pixel colors in RGBA style for input image.
#' 
#' @seealso [mean.qntmap()]
#' 
#' @export
segment <- function(x, ...) {
  if(!is.character(x) | length(x) != 1) stop('x must be a path to the image file')
  UseMethod('segment')
}

#' @rdname segment
#' 
#' @examples
#' library(imager)
#' x <- system.file('extdata/parrots.png',package='imager')
#' print(x) # x is a path to the example image file. 
#' head(segment(x)) # [1] "#747458" "#757559" "#78785C" "#77765B" "#78785C" "#78785C"
#' \dontrun{
#' segment(matrix(1:9, 3, 3)) 
#' # This gives error as current version only support a path of a image file as input
#' }
#' 
#' @importFrom grDevices rgb
#' @importFrom imager load.image
#' @importFrom pipeR pipeline
#' @importFrom purrr pmap
#' @importFrom stats setNames
#'  
#' @export
segment.character <- function(x, ...) {
  pipeline({
    load.image(x)[,,1,]
    apply(3, list)
    lapply(lapply, t)
    setNames(names(formals(rgb))[seq_along(.)])
    pmap(rgb)
    unlist(use.names = FALSE)
  })
}
