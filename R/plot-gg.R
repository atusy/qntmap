#' Draw a histgram for numeric vector based on Scott's choice
#' @importFrom grDevices nclass.FD
#' @importFrom ggplot2 aes coord_cartesian
#' @importFrom ggplot2 element_blank element_rect
#' @importFrom ggplot2 geom_bar geom_histogram ggplot
#' @importFrom ggplot2 theme theme_classic 
#' @importFrom ggplot2 scale_fill_manual
#' @noRd
gghist <- function (x, .min = NA_real_, .max = NA_real_, colors) {
  if(is.numeric(x)) {
    if(!is.finite(.min)) .min <- min(x)
    if(!is.finite(.max)) .max <- max(x)
    x <- x[.min <= x & x <= .max]
    layers <- list(
      geom_histogram(aes(x, fill = stat(x)), bins = nclass.FD(x)),
      ggfill[[match.arg(colors)]]
    )
  } else {
    n <- length(unique(x))
    layers <- list(
      geom_bar(aes(x, y = stat(count / sum(count)), fill = x), color = "black"),
      scale_fill_manual(
        values = mycolors(palette = "pcol", n = n)
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
#' @importFrom ggplot2 aes coord_fixed
#' @importFrom ggplot2 geom_raster ggplot
#' @importFrom ggplot2 guides guide_colorbar guide_legend
#' @importFrom ggplot2 scale_y_reverse scale_fill_manual
#' @importFrom ggplot2 theme theme_classic 
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

#' @importFrom ggplot2 scale_fill_viridis_c scale_fill_gradient
#' @noRd
ggfill <- list(
    viridis = scale_fill_viridis_c(),
    gray = scale_fill_gradient(low = "black", high = "white")
  )

formals(ggheat)$colors <- formals(gghist)$colors <- names(ggfill)
