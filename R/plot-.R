# © 2018 JAMSTEC

#' @name plot
#' 
#' @title Plot methods for `qntmap` package
#' @description
#'   S3 methods to plot object with original classes in `qntmap` package. 
#'   See [`graphics::plot()`] for general use of `plot`. 
#'   Mapping data (`qm_xmap` and `qm_qntmap` classes) are visualized by heat maps. 
#' 
#' @param x 
#'   A `qm_xmap` or `qntmap` class objects returned by 
#'   [read_xmap()], [quantify()], or [qntmap()].
#' @param y
#'   A string specifying a component of `x` to determine colors to fill the map.
#' @param z
#'   A z-value for heatmap.
#' @param colors
#'   A color pallete to use. Either "viridis" (default) or "gray".
#' @param interactive
#'   `TRUE` (default) produces plots with shiny WebUI, and 
#'   `FALSE` produces plots with [`ggplot2::ggplot()`].
#' @param ... 
#'   Ignored
#' 
#' @seealso [`graphics::plot()`]
#' 
#' @importFrom graphics plot
NULL

#' @rdname plot
#' @examples
#' # qm_raster class object
#' d <- data.frame(expand.grid(x=1:5,y=1:5), fill = runif(5))
#' class(d) <- c('qm_raster', class(d))
#' plot(d, 'fill', interactive = FALSE)
#' 
#' @export
plot.qm_raster <- function(
  x, 
  y = setdiff(names(x), c('x', 'y'))[1], 
  z = x[[y]],
  colors = c("viridis", "gray", "discrete"),
  interactive = TRUE, 
  ...
) {
  if (any(c('x', 'y') %nin% names(x))) 
    stop ('Column x or y not found')
  if (interactive) 
    return (plot_shiny(x, y, pcol = colors == "viridis", ...))
  z_is_num <- is.numeric(z)
  zlim <- `if`(z_is_num, range(z), levels(z))
  colors <- match.arg(colors)
  gg_img(
    as_img(lookup[[colors]](z), max(x$y), max(x$x)), 
    zlim = zlim, colors = colors,
    ...
  )
}

#' @rdname plot
#' @examples 
#' # qm_xmap class object
#' xm <- list(A = as.data.frame(matrix(runif(25), 5)))
#' class(xm) <- c('qm_xmap', 'list')
#' plot(xm, interactive = FALSE)
#' 
#' @importFrom dplyr bind_cols
#' @export
plot.qm_xmap <- function(x, y = setdiff(names(x), c('x', 'y'))[1], ...) {
  plot.qm_raster(
    bind_cols(
      expand.grid(
        y = seq(1, nrow(x[[1]])),
        x = seq(1, ncol(x[[1]]))
      )[c("x", "y")],
      lapply(x, unlist, use.names = FALSE)
    ),
    y = y, ...
  ) 
}

#' @rdname plot
#' @examples 
#' # qntmap class object
#' qm <- list(A = list(wt = as.data.frame(matrix(runif(25), 5))))
#' class(qm) <- c( 'qntmap', 'list')
#' plot(qm, interactive = FALSE)
#' 
#' @export
plot.qntmap <- function(
  x, y = setdiff(names(x), c('x', 'y'))[1], ...
) {
  plot.qm_xmap(
    lapply(lapply(x, `[[`, 'wt'), round, 2), y = y, ...
  )
}

# © 2018 YASUMOTO Atsushi
#' @rdname plot
#' @examples
#' # qm_cluster class object
#' cls <- list(
#'   cluster = letters[sample.int(3, 9, replace = TRUE)], 
#'   dims = c(3, 3)
#' )
#' class(cls) <- "qm_cluster"
#' plot(cls, interactive = FALSE)
#' 
#' @importFrom dplyr mutate select
#' @importFrom stats setNames
#' @export
plot.qm_cluster <- function(x, y = NULL, colors =  "discrete", ...) {
  lapply(x$dims, seq) %>>%
    setNames(c("y", "x")) %>>%
    expand.grid %>>%
    mutate(Phase = as.factor(!!x$cluster)) %>>%
    select(x, y, Phase) %>>%
    plot.qm_raster(y = "Phase", colors = "discrete", ...)
} 
