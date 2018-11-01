# © 2018 JAMSTEC

#' @name plot
#' 
#' @title Plot methods for `qntmap` package
#' @description
#' S3 methods to plot object with original classes in `qntmap` package. 
#' See [graphics::plot()] for general use of `plot`. 
#' Mapping data (`qm_xmap` and `qm_qntmap` classes) are visualized by heat maps. 
#' 
#' @param x 
#' `qm_xmap` or `qntmap` class objects returned by 
#' [read_xmap()], [quantify()], or [qntmap()].
#' @param y
#' A string specifying a component of `x` to determine colors to fill the map.
#' @param legend_fill
#' A string to specify legend name for fill. 
#' Default value is taken from `y`.
#' @param interactive
#' `TRUE` returns `plotly` object (default), and 
#' `FALSE` returns `ggplot` object.
#' @param ... ignored
#' @param shiny See plots using Shiny (default: `FALSE`)
#' 
#' @seealso [graphics::plot()]
#' 
#' @importFrom graphics plot
NULL

#' @noRd
#' @examples
#' # qm_raster class object
#' d <- data.frame(x = sample.int(5), y = sample.int(5), fill = runif(5))
#' class(d) <- c('qm_raster', class(d))
#' plot(d, 'fill')
#' 
#' @importFrom stats setNames
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 geom_raster
#' @importFrom ggplot2 coord_fixed
#' @importFrom ggplot2 ggtitle
#' @importFrom ggplot2 scale_y_reverse
#' @importFrom ggplot2 scale_fill_viridis_c
#' @importFrom plotly ggplotly
plot.qm_raster <- function(
  x, y = setdiff(names(x), c('x', 'y'))[1], legend_fill = y, interactive = TRUE, ..., shiny = FALSE
) {
  nm <- names(x)
  
  if(any(c('x', 'y') %nin% nm)) stop('data.frame must contain column x and y')
  
  args <- setNames(lapply(nm, as.name), nm)
  names(args)[names(args) == y] <- 'fill'
  aes_fix <- do.call(aes, args)
  
  g <- ggplot(data = as.data.frame(x), mapping = aes_fix) +
    geom_raster() +
    coord_fixed() +
    ggtitle(y) +
    scale_y_reverse() +
    scale_fill_viridis_c(name = legend_fill)
    # scale_fill_gradientn(
    #   name = legend_fill, 
    #   colors = c('black','purple','blue','green','red','white')
    # )
  
  if(shiny) return(plot_shiny(x, y, interactive))
  if(interactive) return(ggplotly(g))
  
  g
}

#' @rdname plot
#' @examples 
#' # qm_xmap class object
#' xm <- list(A = as.data.frame(matrix(runif(25), 5)))
#' class(xm) <- c('qm_xmap', 'list')
#' plot(xm)
#' 
#' @importFrom pipeR pipeline 
#' @export
plot.qm_xmap <- function() {
  pipeline({
    dim(x[[1]])
    setNames(c('y', 'x'))
    lapply(seq)
    expand.grid()
    list()
    c(lapply(x, unlist, use.names = FALSE))
    as.data.frame()
    `class<-`(c('qm_raster', class(.)))
    plot(y = y, legend_fill = legend_fill, interactive = interactive, ..., shiny = shiny)
  })
}
formals(plot.qm_xmap) <- formals(plot.qm_raster)


#' @rdname plot
#' @examples 
#' # qntmap class object
#' qm <- list(A = list(wt = as.data.frame(matrix(runif(25), 5))))
#' class(qm) <- c( 'qntmap', 'list')
#' plot(qm)
#' 
#' @importFrom pipeR pipeline 
#' @export
plot.qntmap <- function() {
  pipeline({
    x
    lapply(`[[`, 'wt')
    lapply(round, 2)
    `class<-`(c('qm_xmap', 'list'))
    plot(y = y, legend_fill = legend_fill, interactive = interactive, ..., shiny = shiny)
  })
}
formals(plot.qntmap) <- formals(plot.qm_raster)
