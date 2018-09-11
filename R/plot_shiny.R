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
#' @importFrom stringr str_replace
#' 
#' @noRd
plot_shiny <- function(x, y = setdiff(names(x), c('x', 'y'))[1], interactive = TRUE) {

  nm <- names(x)

  U <- shinyUI(fluidPage(
    
    sidebarLayout(
      sidebarPanel(
        splitLayout(
          selectInput('fill', 'Element', setdiff(nm, c('x', 'y')), selected = y),
          numericInput('min', 'Min', value = NA),
          numericInput('max', 'Max', value = NA),
          actionButton("goButton", "", icon("refresh"))
        ),
        plotOutput('hist'),
        br(),
        numericInput('height', 'Height of graph [px]', value = 800),
        br(),
        checkboxInput('interactive', 'Interactive', value = interactive)
      ),
      
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
      )
    ),
    tags$style(type='text/css', "#goButton { width:100%; margin-top: 25px;}")
  ))
  
  S <- shinyServer(function(input, output) {

    g_raster <- reactive({
      input$goButton
      isolate(
        ggplot(
          mutate_at(
            x,
            input$fill,
            function(i) {
              if(is.finite(input$min) && min(i) < input$min && input$min < max(i)) {
                i[i < input$min] <- input$min
              }
              if(is.finite(input$max) && min(i) < input$max && input$max < max(i)) {
                i[i > input$max] <- input$max
              }
              i
            }
          ),
          do.call(
            aes,
            lapply(
              setNames(nm, str_replace(nm, paste0('^', input$fill, '$'), 'fill')), 
              as.name
            )
          )
        )
      )
    })
      
    output$plot <- renderPlot(
      g_raster() + layers_raster,
      height = reactive(input$height)
    )
    output$plotly <- plotly::renderPlotly(
      plotly::ggplotly(
        g_raster() + layers_raster,
        height = input$height
      ) 
    )
    
    output$click <- renderPrint({
      d <- event_data("plotly_click")
      if (is.null(d)) 
        cat('Click pixel & keep data here')
      else
        x[x$x == d$x & x$y == -d$y, ]
    })
    
    output$hist <- renderPlot(
      x[[input$fill]] %>>%
        `[`(`if`(is.finite(input$min) && input$min < max(.), . > input$min, TRUE)) %>>%
        `[`(`if`(is.finite(input$max) && input$max > min(.), . < input$max, TRUE)) %>>%
        gghist()
    )
  
  })
  
  runApp(appDir = list(server = S, ui = U))
  
}

#' Layers for plotting mapping data
#' @importFrom ggplot2 geom_raster
#' @importFrom ggplot2 coord_fixed
#' @importFrom ggplot2 scale_fill_gradientn
#' @importFrom ggplot2 theme_classic
#' @importFrom ggplot2 theme
#' @noRd
layers_raster <- list(
  ggplot2::geom_raster(),
  ggplot2::coord_fixed(),
  ggplot2::theme_classic(),
  ggplot2::scale_y_reverse(),
  ggplot2::scale_fill_gradientn(
    colors = c('black','purple','blue','green','red','white')
  )
)

#' Layers for plotting mapping histograms
#' @importFrom ggplot2 geom_col
#' @importFrom ggplot2 scale_fill_gradientn
#' @importFrom ggplot2 theme_classic
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 element_blank
#' @noRd
layers_hist <- list(
  ggplot2::geom_col(),
  ggplot2::scale_fill_gradientn(
    colors = c('black','purple','blue','green','red','white')
  ),
  ggplot2::theme_classic(),
  ggplot2::theme(legend.position = 'none', axis.title = ggplot2::element_blank())
)

#' Daraw a histgram for numeric vector based on Scott's choice
#' @importFrom graphics hist
#' @importFrom dplyr mutate
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 aes
#' @noRd
gghist <- function(x) {
  x %>>%
    hist(breaks = 'Scott', plot = FALSE) %>>%
    `[`(c('mids', 'counts')) %>>%
    as.data.frame %>>%
    mutate(w = mids[2] - mids[1]) %>>%
    ggplot(aes(mids, counts, fill = mids, width = w)) +
    layers_hist
}
