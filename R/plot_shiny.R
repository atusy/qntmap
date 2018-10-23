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
#' @importFrom stringr str_replace
#' 
#' @noRd
plot_shiny <- function(x, y = setdiff(names(x), c('x', 'y'))[1], interactive = TRUE) {

  nm <- names(x)
  aes_fix <- do.call(aes, lapply(nm, as.name))

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
          data = 
            mutate_at(
              x,
              input$fill,
              function(d) {
                m_d <- min(d)
                M_d <- max(d)
                m_in <- input$min
                M_in <- input$max
                if(is.finite(m_in) && m_d < m_in && m_in < M_d) 
                  d[d < m_in] <- m_in
                if(is.finite(M_in) && M_d < M_in && M_in < M_d) 
                  d[d > M_in] <- M_in
                d
              } # function
            ), # mutate_at
          mapping = 
            # aes_string(x = 'x', y = 'y', fill = input$fill) # faster but less info
            setNames(aes_fix, str_replace(nm, paste0('^', input$fill, '$'), 'fill'))
        ) # ggplot
      ) # isolate
    }) # reactive
      
    output$plot <- renderPlot(
      g_raster() + layers_raster,
      height = reactive(input$height)
    ) # renderPlot
    
    output$plotly <- plotly::renderPlotly(
      ggplotly(
        g_raster() + layers_raster,
        height = input$height
      ) 
    ) # renderPlotly
    
    output$click <- renderPrint({
      d <- event_data("plotly_click")
      `if`(
        is.null(d), 
        cat('Click pixel & keep data here'),
        unlist(x[x$x == d$x & x$y == -d$y, ])
      )
    }) # renderPrint
    
    output$hist <- renderPlot(pipeline({
      x[[input$fill]]
        `[`(
          `if`(
            is.finite(input$min) && input$min < max(.), 
            . > input$min, 
            TRUE
          ) &
          `if`(
            is.finite(input$max) && input$max > min(.),
            . < input$max, 
            TRUE
          )
        ) # [
        gghist()
    })) # pipeline, renderPlot
  
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
  ggplot2::scale_fill_gradientn(
    colors = c('black','purple','blue','green','red','white')
  ),
  ggplot2::theme_classic(),
  ggplot2::theme(
    plot.background = element_rect(fill = '#f5f5f5', color = '#f5f5f5'),
    panel.background = element_rect(fill = '#f5f5f5', color = '#f5f5f5'),
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
