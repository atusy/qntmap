# © 2018 JAMSTEC

#' Plot mapping data using shiny
#' 
#' @inheritParams plot.qm_raster
#' 
#' @import shiny
#' @importFrom plotly renderPlotly
#' @importFrom plotly plotlyOutput 
#' @importFrom plotly ggplotly
#' @importFrom plotly event_data
#' @importFrom graphics hist
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 aes
#' @importFrom dplyr mutate_at
#' @importFrom scales squish
#' @importFrom stringr str_replace
#' 
#' @noRd
plot_shiny <- function(x, y = setdiff(names(x), c('x', 'y'))[1], interactive = TRUE) {

  nm <- names(x)


  U <- shinyUI(fluidPage(
    
    sidebarLayout(
      sidebarPanel(
        splitLayout(
          selectInput('fill', 'Element', setdiff(nm, c('x', 'y')), selected = y, selectize = FALSE),
          numericInput('min', 'Min', value = NA),
          numericInput('max', 'Max', value = NA),
          actionButton("goButton", "", icon("refresh"))
        ),
        plotOutput('hist'), br(),
        splitLayout('Height of graph [px]', numericInput('height', NULL, value = 800)), br(),
        checkboxInput('interactive', 'Interactive', value = interactive)
      ), # sidebarPanel
      
      mainPanel(
        conditionalPanel(
          condition = "input.interactive == true",
          plotlyOutput("plotly", height = 'auto')
        ),
        conditionalPanel(
          condition = "input.interactive == true",
          verbatimTextOutput("click")
        ),
        conditionalPanel(
          condition = "input.interactive == false",
          plotOutput("plot", height = 'auto')
        )
      ) # mainPanel
    ), # sidebarLayout
    
    tags$style(type='text/css', "#goButton { width:100%; margin-top: 25px;}")
    
  )) # fluidPage, shinyUI
  
  S <- shinyServer(function(input, output) {
    
    g_raster <- reactive({
      input$goButton
      isolate(
          ggplot(
            mutate_at(
              x[c('x', 'y', input$fill)],
              input$fill, trim, .min = input$min, .max = input$max
            ), # mutate_at
            aes_string('x', 'y', fill = input$fill) # faster but less info
          ) + layers_raster # ggplot
      ) # isolate
    }) # reactive
      
    output$plot <- renderPlot(g_raster(), height = reactive(input$height))

    heatmap <- reactive({
      input$goButton
      isolate(plotly_heatmap(
        x[['x']], x[['y']], squish(x[[input$fill]], c(input$min, input$max)),
        title = input$fill, height = input$height
      ))
    })
    
    output$plotly <- plotly::renderPlotly(heatmap())
    
    output$click <- renderPrint({
        d <- event_data("plotly_click")
        `if`(
          is.null(d), 
          cat('Click pixel & keep data here'),
          unlist(x[x[['x']] == d[['x']] & x[['y']] == d[['y']], ])
        )
      }) # renderPrint
    
    output$hist <- renderPlot(
        gghist(x[[input$fill]], input[['min']], input[['max']])
      )
  
  })
  
  runApp(appDir = list(server = S, ui = U))
  
}

#' Layers for plotting mapping data
#' @importFrom ggplot2 geom_raster
#' @importFrom ggplot2 coord_fixed
#' @importFrom ggplot2 scale_fill_gradientn
#' @importFrom ggplot2 scale_fill_viridis_c
#' @importFrom ggplot2 theme_classic
#' @importFrom ggplot2 theme
#' @noRd
layers_raster <- list(
  ggplot2::geom_raster(),
  ggplot2::coord_fixed(),
  ggplot2::theme_classic(),
  ggplot2::scale_y_reverse(),
  ggplot2::scale_fill_viridis_c(),
  # ggplot2::scale_fill_gradientn(
  #   colors = c('black','purple','blue','green','red','white')
  # ),
  NULL
)

#' Layers for plotting mapping histograms
#' @importFrom ggplot2 geom_col
#' @importFrom ggplot2 scale_fill_gradientn
#' @importFrom ggplot2 theme_classic
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 element_blank
#' @importFrom ggplot2 element_rect
#' @noRd
layers_hist <- list(
  ggplot2::scale_fill_viridis_c(),
  # ggplot2::scale_fill_gradientn(
  #   colors = c('black','purple','blue','green','red','white')
  # ),
  ggplot2::theme_classic(),
  ggplot2::theme(
    plot.background = ggplot2::element_rect(fill = '#f5f5f5', color = '#f5f5f5'),
    panel.background = ggplot2::element_rect(fill = '#f5f5f5', color = '#f5f5f5'),
    legend.position = 'none', 
    axis.title = ggplot2::element_blank()
  )
)

#' Daraw a histgram for numeric vector based on Scott's choice
#' @importFrom graphics hist
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 aes
#' @noRd
gghist <- function(x, .min = NA, .max = NA) {
  if(!is.finite(.min)) .min <- min(x)
  if(!is.finite(.max)) .max <- max(x)
  d <- as.data.frame(hist(
      x[.min <= x & x <= .max], 
      breaks = 'Scott', plot = FALSE
    )[(c('mids', 'counts'))])
  ggplot(d, aes(mids, counts, fill = mids)) +
    geom_col(width = d$mids[2] - d$mids[1]) +
    layers_hist
}


#' heatmap using plotly
#' @importFrom plotly layout
#' @importFrom plotly plot_ly
#' @noRd
plotly_heatmap <- function(x, y, z, title = '', ...) {
  layout(
    plot_ly(
      x = x, y = y, z = z, type = 'heatmap',
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

