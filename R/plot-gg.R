#' Draw a histgram for numeric vector based on Scott's choice
#' @name gghist
#' @param x An atomic vector
#'
#' @importFrom graphics hist
#' @noRd
gghist <- function(x, ...) {
  UseMethod("gghist")
}

#' @rdname gghist
#' @param .min,.max
#'   A minimum and a maximum values (default: `NA_real`).
#'   `NA_real_` will be replaced by minimum and maximum values of x.
#'   Values outlying x will be squished.
#' @param colors
#'   "viridis" or "gray"
#' @noRd
gghist.numeric <- function(x, .min = NA_real_, .max = NA_real_, colors) {
  range_x <- range(x, na.rm = TRUE)
  if (!is.finite(.min) || .min < range_x[1]) .min <- range_x[1]
  if (!is.finite(.max) || .max > range_x[2]) .max <- range_x[2]

  freq <- hist(x[.min <= x & x <= .max], breaks = "FD", plot = FALSE)
  width <- freq$breaks[[2L]] - freq$breaks[[1L]]
  
  df_col <- data.frame(x = freq$mids, y = freq$counts)
  df_lim <- data.frame(x = c(.min, .max), y = c(0L, 0L))
  
  ggplot(df_col) +
    aes(.data$x, .data$y, width = {{ width }}, fill = .data$x, color = .data$x) +
    geom_col(
      color = "gray", fill = "transparent", size = 2, 
      show.legend = FALSE, position = "identity"
    ) +
    geom_col(data = rbind(df_col, df_lim), show.legend = FALSE, position = "identity") +
    scale_fill[[match.arg(colors)]]() +
    scale_color[[match.arg(colors)]]() +
    gghist_theme(ylim = c(0L, max(freq$counts) + 1L))
}

#' @rdname gghist
#' @noRd
gghist.character <- function(x, ...) {
  gghist.factor(as.factor(x), ...)
}

#' @rdname gghist
#' @noRd
gghist.factor <- function(x, ...) {
  ggplot(data.frame(x = x)) +
    geom_bar(aes(x, y = stat(.data$count / sum(.data$count)), fill = x), color = "black") +
    scale_fill_manual(values = rgb(lookup$discrete(levels(x)))) +
    gghist_theme()
}

#' gghist: theme
#' @noRd
gghist_theme <- function(xlim = NULL, ylim = NULL) {
  list(
    coord_cartesian(xlim = xlim, ylim = ylim, expand = FALSE),
    theme_classic(),
    theme(
      legend.position = "none",
      axis.title = element_blank()
    )
  )
}

#' Color palette
#' @importFrom scales gradient_n_pal viridis_pal
#' @importFrom grDevices colorRamp col2rgb
#' @noRd
palette <- list(
  # A rgb matrix
  viridis = t(col2rgb(unique(gradient_n_pal(viridis_pal(
    alpha = 1, begin = 0, end = 1, direction = 1, option = "D"
  )(6))(seq(0, 1, 1e-4))))) / 255,
  # A function returning rgb matrix according to 0--1 input
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
#' @param x An atomic vector
#' @importFrom scales rescale
#' @noRd
lookup <- list(
  viridis  = function(x, to, ...) palette$viridis[rescale(x, to = to, ...), , drop = FALSE],
  gray     = function(x, ...) rep(rescale(x, ...), 3L),
  discrete = function(x, ...) {
    x <- as.factor(x)
    (palette$discrete(rescale(seq_along(levels(x)))) / 255L)[as.integer(x), , drop = FALSE]
  }
)
formals(lookup$viridis)$to <- c(1L, nrow(palette$viridis))

#' Convert to array
#' @param x A value returned by `lookup()`
#' @param row Number of rows
#' @param col Number of columns
#' @noRd
as_img <- function(x, row, col) {
  structure(x, .Dim = c(row, col, 3L))
}

#' Choice of scales for filling
#' @noRd
scale_fill <- list(
  gray = function(...) scale_fill_gradient(..., low = "black", high = "white"),
  viridis = scale_fill_viridis_c,
  discrete = scale_fill_manual
)

#' Choice of scales for coloring
#' @noRd
scale_color <- list(
  gray = function(...) scale_color_gradient(..., low = "black", high = "white"),
  viridis = scale_color_viridis_c,
  discrete = scale_color_manual
)

#' Raster image with ggplot2::annotation_raster
#' 
#' @param img A value returned by `as_img`
#' @param xlim,ylim,zlim Limits of x, y, and z
#' @param zname Name of z (title of scale_fill)
#' @param colors A palette of colros to use. If manual, specify `values` in `...`
#' @param barheight Barheight for continuous scale
#' @param ... Other arguments passed to `scale_fill_*`
#' 
#' @importFrom grid unit
#' @noRd
gg_img <- function(
                   img,
                   xlim = c(0, NCOL(img)) + 0.5,
                   ylim = c(0, NROW(img)) + 0.5,
                   zlim = c(0, 1),
                   zname = NULL,
                   colors = c("viridis", "gray", "discrete"),
                   barheight = unit(1, "npc") - unit(4, "line"),
                   ...
) {
  is_z_num <- is.numeric(zlim)
  colors <- `if`(is_z_num, match.arg(colors), "discrete")
  ggplot(data.frame(x = 0, y = 0, fill = zlim)) +
    aes(.data$x, .data$y, fill = .data$fill) +
    geom_tile(width = 0, height = 0) + # Invisible tile for legend
    coord_fixed(xlim = xlim, ylim = ylim, expand = FALSE) +
    annotation_raster(
      img, xmin = xlim[1L], xmax = xlim[2L], ymin = -ylim[2L], ymax = -ylim[1L]
    ) +
    scale_y_reverse() +
    `if`(
      is_z_num,
      list(
        scale_fill[[colors]](zname, ...),
        guides(fill = guide_colorbar(barheight = barheight))
      ),
      scale_fill[[colors]](name = NULL, values = rgb(lookup$discrete(zlim)), ...)
    )
}

formals(gg_img)$colors <- formals(gghist.numeric)$colors <- names(scale_fill)
