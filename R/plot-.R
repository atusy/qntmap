#' @name plot-qntmap
#'
#' @title Plot methods for `qntmap` package
#' @description
#'  S3 methods to plot object with original classes in `qntmap` package.
#'  See [`graphics::plot()`] for general use of `plot`.
#'  Mapping data (`qm_xmap` and `qm_qntmap` classes) are visualized by heat maps.
#'
#' @param x 
#'  An object of class `qntmap`, `qm_cluster`, or `qm_xmap`,
#'  returned by [quantify()], [qntmap()], and [read_xmap()], respectively.
#' @param zname,y
#'  A string specifying a component of `x` to determine colors to fill the map.
#'  `y` is the alias of `zname`.
#' @param zlim
#'  A range of z.
#' @param colors
#'  A color scale "viridis" (default) or "gray" applicable when fill is continuous.
#' @param interactive
#'  `TRUE` (default) produces plots with shiny WebUI, and
#'  `FALSE` produces plots with [`ggplot2::ggplot()`].
#' @param ...
#'  Arguments passed to internal functions.
#'
#' @seealso [`graphics::plot()`]
#'
#' @importFrom graphics plot
NULL

#' @rdname plot-qntmap
#' @examples
#' # qm_raster class object
#' d <- data.frame(expand.grid(x = 1:5, y = 1:5), fill = runif(5))
#' class(d) <- c("qm_raster", class(d))
#' plot(d, "fill", interactive = FALSE)
#' @export
plot.qntmap <- function(
                        x,
                        y = setdiff(names(x), c("x", "y"))[1L],
                        zname = y,
                        zlim = NULL,
                        colors = c("magma", "viridis", "gray"),
                        interactive = TRUE,
                        ...
) {
  if (interactive) return(plot_shiny(x, y, pcol = colors == "viridis", ...))

  print(autoplot(
    object = x, zname = zname, zlim = zlim, colors = match.arg(colors), ...
  ))
}

#' @rdname plot-qntmap
#' @export
plot.qm_xmap <- plot.qntmap

#' @rdname plot-qntmap
#' @export
plot.qm_cluster <- plot.qntmap
formals(plot.qm_cluster)$y <- "cluster"
