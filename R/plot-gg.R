#' @importFrom ggplot2
#'   ggplot
#'   aes
NULL

#' Draw a histgram for numeric vector based on Scott's choice
#' @noRd
#' 
#' @importFrom grDevices nclass.FD
#' @importFrom ggplot2 
#'   coord_cartesian
#'   element_blank 
#'   element_rect
#'   geom_bar
#'   geom_histogram 
#'   theme
#'   theme_classic 
#'   scale_fill_manual
gghist <- function (x, .min = NA_real_, .max = NA_real_, colors) {
  layers <- if(is.numeric(x)) {
    if(!is.finite(.min)) .min <- min(x)
    if(!is.finite(.max)) .max <- max(x)
    x <- x[.min <= x & x <= .max]
    list(
      geom_histogram(aes(x, fill = stat(x)), bins = nclass.FD(x)),
      scale_fill[[match.arg(colors)]]()
    )
  } else {
    list(
      geom_bar(aes(x, y = stat(count / sum(count)), fill = x), color = "black"),
      scale_fill_manual(
        values = rgb(lookup$discrete(as.factor(levels(as.factor(x)))))
      )
    )
  }

  bg <- element_rect(fill = '#f5f5f5', color = '#f5f5f5')
  ggplot(data.frame(x = x)) +
    coord_cartesian(expand = FALSE) +
    theme_classic() +
    theme(
      plot.background = bg,
      panel.background = bg,
      legend.position = 'none', 
      axis.title = element_blank()
    ) +
    layers
}


#' Color palette
#' @importFrom scales
#'   viridis_pal
#'   gradient_n_pal
#' @importFrom grDevices
#'   colorRamp
#' @noRd
palette <- list(
    viridis = gradient_n_pal(viridis_pal(
        alpha = 1, begin = 0, end = 1, direction = 1, option = "D"
      )(6)),
    discrete = colorRamp(
        c("#000000", 
          "#0000FF", "#00FFFF", 
          "#00FF00", "#FFFF00", 
          "#FF0000", "#FF00FF",
          "#EEEEEE"
        ),
        space = "rgb"
      )
  )

#' Look up colors based on palette
#' @param x A numeric value ranging 0 to 1
#' @importFrom grDevices col2rgb
#' @importFrom scales rescale
#' @noRd
lookup <- list(
    gray = identity,
    viridis = function(x) t(col2rgb(palette$viridis(x))) / 255,
    discrete = function(x) {
      palette$discrete(rescale(as.integer(x))) / 255
    }
  )

#' Convert to array
#' @param color A value returned by `lookup()`
#' @param row Number of rows
#' @param col Number of columns
#' @noRd
as_img <- function(color, row, col) array(color, dim = c(row, col, 3L))

#' Choice of scales for filling
#' @importFrom ggplot2
#'   scale_fill_gradient
#'   scale_fill_viridis_c
#'   scale_fill_manual
#' @noRd
scale_fill <- list(
  gray = scale_fill_gradient, # low and high are fixed later
  viridis = scale_fill_viridis_c,
  discrete = scale_fill_manual
)
formals(scale_fill$gray)[c("low", "high")] <- list("black", "white")

#' Raster image with ggplot2::annotation_raster
#' @param img A value returned by `as_img`
#' @param xlim,ylim,zlim Limits of x, y, and z
#' @param zname Name of z (title of scale_fill)
#' @param colors A palette of colros to use. If manual, specify `values` in `...`
#' @param ... Other arguments passed to `scale_fill_*`
#' @importFrom ggplot2
#'   annotation_raster
#'   coord_fixed
#'   geom_tile
#'   scale_y_reverse
#'   guides
#'   guide_colorbar
#' @importFrom grid
#'   unit
#' @noRd
gg_img <- function(
    img, 
    xlim = c(0, NCOL(img)) + 0.5, 
    ylim = c(0, NROW(img)) + 0.5, 
    zlim = c(0, 1), 
    zname = NULL, 
    colors = c("viridis", "gray", "discrete"),
    ...
  ) {
  colors <- `if`(is.numeric(zlim), match.arg(colors), "discrete")
  ggplot(data.frame(x = 0, y = 0, fill = zlim), aes(x, y, fill = fill)) +
    geom_tile(size = 0) +
    coord_fixed(xlim = xlim, ylim = ylim, expand = FALSE) +
    annotation_raster(
      img, xmin = xlim[1], xmax = xlim[2], ymin = -ylim[2], ymax = -ylim[1]
    ) +
    scale_y_reverse() +
    `if`(
      is.numeric(zlim), 
      list(
        scale_fill[[colors]](zname, ...),
        guides(fill = guide_colorbar(
          barheight = unit(1, "npc") - unit(4, "line")
        ))
      ),
      scale_fill[[colors]](
        name = NULL,
        values = rgb(lookup$discrete(as.factor(zlim))), ...
      )
    )
}

formals(gg_img)$colors <- formals(gghist)$colors <- names(scale_fill)
