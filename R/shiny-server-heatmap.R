zlim <- function(id, map, input) {
  req(map)
  ids <- paste0(id, "_", c("elem", "min", "max"))
  z <- range(map[[input[[ids[[1L]]]]]])
  .min <- input[[ids[[2L]]]]
  .max <- input[[ids[[3L]]]]
  c(
    `if`(is.na(.min) || .min < z[1L], z[1L], .min),
    `if`(is.na(.max) || .max > z[2L], z[2L], .max)
  )
}

zquish <- function(id, map, zlim, input) {
  req(map)
  z <- input[[paste0(id, "_elem")]]
  if (is.null(z)) z <- "cluster"
  squish(map[[z]], zlim)
}

raster <- function(input, id, ranges, .margin, x, zlim, step_size = NULL) {
  shiny::req(x)
  ids <- paste0(id, "_", c("elem", "color", "scale"))
  rx <- if (is.null(ranges$x)) ranges$x0 else ranges$x
  ry <- if (is.null(ranges$y)) ranges$y0 else ranges$y
  gg_img(
    x[ry[[1L]]:ry[[2L]], rx[[1L]]:rx[[2L]], ],
    xlim = rx + .margin, ylim = ry + .margin,
    zlim = zlim, zname = input[[ids[[1L]]]],
    colors = input[[ids[[2L]]]], base_size = 16,
    scale = input[[ids[[3L]]]], step_size = step_size
  )
}
