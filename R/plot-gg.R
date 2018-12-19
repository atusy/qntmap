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
      ggfill[[match.arg(colors)]]
    )
  } else {
    list(
      geom_bar(aes(x, y = stat(count / sum(count)), fill = x), color = "black"),
      scale_fill_manual(
        values = mycolors(palette = "pcol", n = length(unique(x)))
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

#' @param x,y,z x-, y-, and z-coordinates
#' @param nm title for legend of fill
#' @param colors A palette to chose when z is continuous.
#' @importFrom ggplot2
#'   coord_fixed
#'   geom_raster 
#'   guides 
#'   guide_colorbar 
#'   guide_legend
#'   scale_y_reverse 
#'   scale_fill_manual
#'   theme_classic 
#' @importFrom grid unit
#' @importFrom rlang sym
#' @importFrom stats setNames
#' @importFrom scales squish
#' @noRd
ggheat <- function (
  x, y, z, nm = "z", colors, 
  range = c(NA_real_, NA_real_), 
  coord = coord_fixed()
) {
  if(is.numeric(z)) z <- squish(z, range)
  ggplot(
    setNames(data.frame(x, y, z), c("x", "y", nm)), 
    aes(x, y, fill = !!sym(nm))
  ) +
    coord +
    geom_raster() +
    theme_classic() +
    scale_y_reverse() +
    if (is.numeric(z)) {
      list(
        ggfill[[match.arg(colors)]],
        guides(fill = guide_colorbar(barheight = unit(1, "npc") - unit(4, "line")))
      )
    } else {
      scale_fill_manual(
        values = mycolors(palette = "pcol", n = length(unique(z)))
      )
    }
}

#' @noRd
#' @importFrom ggplot2 scale_fill_viridis_c scale_fill_gradient
ggfill <- list(
    viridis = scale_fill_viridis_c(),
    gray = scale_fill_gradient(low = "black", high = "white")
  )

formals(ggheat)$colors <- formals(gghist)$colors <- names(ggfill)
