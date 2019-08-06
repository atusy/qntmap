zlim_react <- function(id, map_reactive, input) {reactive({
  req(map_reactive())
  ids <- paste0(id, "_", c("elem", "min", "max"))
  z <- range(map_reactive()[[input[[ids[[1L]]]]]])
  .min <- input[[ids[[2L]]]]
  .max <- input[[ids[[3L]]]]
  c(
    `if`(is.na(.min) || .min < z[1L], z[1L], .min),
    `if`(is.na(.max) || .max > z[2L], z[2L], .max)
  )
})}

squish_react <- function(id, map_reactive, zlim_reactive, input) {reactive({
  req(map_reactive())
  z <- input[[paste0(id, "_elem")]]
  if (is.null(z)) z <- "cluster"
  squish(map_reactive()[[z]], zlim_reactive())
})}

raster <- function(x, ranges, range_x, range_y, .margin, zlim, input, id, step_size = NULL) {
  ids <- paste0(id, "_", c("elem", "color", "scale"))
  rx <- if (is.null(ranges$x)) range_x else ranges$x
  ry <- if (is.null(ranges$y)) range_y else ranges$y
  gg_img(
    x[ry[[1L]]:ry[[2L]], rx[[1L]]:rx[[2L]], ],
    xlim = rx + .margin, ylim = ry + .margin,
    zlim = zlim, zname = input[[ids[[1L]]]],
    colors = input[[ids[[2L]]]], base_size = 16,
    scale = input[[ids[[3L]]]], step_size = step_size
  )
}
