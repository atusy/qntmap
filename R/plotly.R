#' heatmap using plotly
#' @importFrom plotly layout
#' @importFrom plotly plot_ly
#' @noRd
plotly_heatmap <- function(x, y, z, title = '', ...) {
  layout(
    plot_ly(
      x = x, y = y, z = z, type = 'heatmap',
      colors = viridis,
      colorbar = list(title = title, len = 1),
      ...
    ),
    xaxis = xaxis, yaxis = yaxis
  )
}

# xaxis for plotly_heatmap
xaxis <- list(
  rangemode = 'tozero',
  showgrid = FALSE,
  zeroline = FALSE
)

# yaxis for plotly_heatmap
yaxis <- c(
  xaxis,
  scaleanchor = 'x', 
  autorange = 'reversed'
)

#' @importFrom scales gradient_n_pal
# scales::gradient_n_pal

#' @importFrom scales viridis_pal
# scales::viridis_pal

#' @noRd
viridis <- getExportedValue('scales', 'gradient_n_pal')(
    getExportedValue('scales', 'viridis_pal')(
      alpha = 1, begin = 0, end = 1, direction = 1, option = 'viridis'
    )(6), 
    values = NULL, space = "Lab"
  )

