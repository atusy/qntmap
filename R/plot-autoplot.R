#' @export
ggplot2::autoplot

#' Create a ggplot object for objects created by qntmap package
#' 
#' @name autoplot-qntmap
#' 
#' @inheritParams ggplot2::autoplot
#' @param zname 
#'   Name of z-axis (fill).
#'   If unset, first column except "x" and "y" is chosen for objects with 
#'   `qm_xmap` or `qntmap` classes, and "cluster" is chosen for the `qm_cluster`
#'   class object.
#' @param zlim Range limit of z-coordinates. This is neglected if z is discrete.
#' @param colors If z is numeric, "viridis" or "gray" are the choice.
#' 
#' @importFrom scales squish
autoplot.qntmap <- function(
  object, zname = setdiff(names(object), c("x", "y"))[[1L]], 
  zlim = NULL, colors = c("viridis", "gray"), ...
) {

  z <- object[[zname]]

  if (is.numeric(z)) {
    colors <- match.arg(colors)
    zlim <- if (is.null(zlim)) range(z) else squish(zlim, range(z))
    z <- squish(z, range = zlim)
  } else {
    colors <- "discrete"
    zlim <- if (is.factor(z)) levels(z) else sort(unique(z))
  }

  gg_img(
    as_img(lookup[[colors]](z, from = zlim), max(object$y), max(object$x)),
    zlim = zlim, zname = zname, colors = colors, ...
  )
}

#' @rdname autoplot-qntmap
#' @export
autoplot.qm_xmap <- autoplot.qntmap

#' @rdname autoplot-qntmap
#' @export
autoplot.qm_cluster <- autoplot.qntmap
formals(autoplot.qm_cluster)$zname <- "cluster"
