#' Plot mapping data using shiny
#'
#' @inheritParams plot.qm_raster
#'
#' @importFrom DT dataTableOutput
#' @importFrom htmltools br tags
#' @importFrom shinyWidgets radioGroupButtons
#' @importFrom shiny 
#'   brushOpts 
#'   checkboxInput 
#'   fluidPage 
#'   hoverOpts htmlOutput 
#'   mainPanel 
#'   numericInput 
#'   plotOutput 
#'   selectInput sidebarLayout sidebarPanel splitLayout splitLayout 
#'   uiOutput
#' @noRd
ui <- function(elm, selected = elm[[1]], pcol = TRUE) {fluidPage(
  sidebarLayout(
    sidebarPanel(
      splitLayout(
        selectInput('fill', 'Element', elm, selected = selected, selectize = FALSE),
        numericInput('min', 'Min', value = NA_real_),
        numericInput('max', 'Max', value = NA_real_)#,
      ),
      plotOutput('hist'), br(),
      splitLayout(
        'Height of graph [px]', numericInput('height', NULL, value = 600)
      ),
      checkboxInput('pcol', 'Pseudocolor', value = pcol)
    ),
    mainPanel(
      tags$head(tags$style('
         #tip {
          position: absolute;
          width: auto;
          z-index: 100;
         }
      ')),
      tags$script('
        $(document).ready(function(){
          // id of the plot
          $("#heatmap").mousemove(function(e){ 
    
            // ID of uiOutput
            $("#tip").show();         
            $("#tip").css({             
              top: (e.pageY + 5) + "px",             
              left: (e.pageX + 5 - $("#heatmap").offset().left) + "px"         
            });     
          });     
        });
      '),
      radioGroupButtons(
        inputId = "mouse",
        label = "Mouse actions",
        choices = c("Zoom", "Move", "Summarize"),
        selected = "Zoom",
        status = "primary"
      ),
      htmlOutput("mouseHelp"),
      plotOutput(
        "heatmap", 
        hover = hoverOpts(id = "hover"),
        dblclick = "click",
        brush = brushOpts(id = "brush", resetOnNew = TRUE),
        height = 'auto'
      ),
      uiOutput("tip"),
      dataTableOutput("dt")
    )
  )
)}











#' @importFrom DT renderDT
#' @importFrom dplyr everything mutate select summarize_if
#' @importFrom scales squish
#' @importFrom shiny 
#'   htmlOutput reactive reactiveValues observeEvent 
#'   renderPlot renderPrint renderUI req 
#' @noRd
server <- function(data) {
    range_x <- range(data$x)
    range_y <- range(data$y)

    .env <- new.env()
    .env$log <- data %>>% 
      summarize_if(is.numeric, mean) %>>% 
      mutate(ID = 0L, Area = "Whole") %>>% 
      select(Area, x, y, everything())
    .env$id <- 0L
    
    function(input, output) {

      colors <- reactive(`if`(input$pcol, "viridis", "gray"))
      
      output$hist <- renderPlot(gghist(
        data[[input$fill]], input[['min']], input[['max']], colors = colors()
      ))
      
      output$mouseHelp <- renderPrint(cat(msg$mouseHelp[input$mouse]))
      
      ranges <- reactiveValues(x = NULL, y = NULL)
      
      observeEvent(input$click, {
        if (input$mouse == "Zoom")
          if (!is.null(input$brush)) {
            ranges$x <- squish(c(input$brush$xmin, input$brush$xmax), range_x)
            ranges$y <- squish(c(input$brush$ymin, input$brush$ymax), range_y)
          } else {
            ranges$x <- ranges$y <- NULL
          }
        if (input$mouse == "Move")
          if(!is.null(ranges$x)) {
            ranges$x <- ranges$x + input$click$x - mean(ranges$x)
            ranges$y <- ranges$y + input$click$y - mean(ranges$y)
          }
      })

      hm <- reactive({
        ggheat(
          data[['x']], data[['y']], data[[input$fill]], nm = input$fill,
          colors = colors(), range = c(input$min, input$max), coord = NULL
        )
      })
      
      output$heatmap <- renderPlot(
        hm() + coord_fixed(xlim = ranges$x, ylim = ranges$y),
        height = reactive(input$height)
      )
      
      hover <- reactive(pick_hover(data, input$hover, input$fill))
      
      output$tip <- renderUI({
        req(nrow(hover()) == 1L) # Suppress rendering if hovering outside plot
        htmlOutput(
          "vals", style = "background-color:#DDDDDDDD; font-family:monospace"
        )
      })
      
      output$vals <- renderPrint(format_hover(hover()))
      
      dt <- reactive({
        if(input$mouse != "Summarize") 
          return(format_summary(.env$log))
        if(!is.null(input$brush))
          return(summarize_box(data, input$brush, .env))
        if(!is.null(input$click))
          return(summarize_click(data, input$click, .env))
        format_summary(.env$log)
      })
      
      output$dt <- renderDT(dt())
      
    }
}















#' @noRd
msg <- list(
  mouseHelp = c(
    Zoom = "Zoom by double click selected area. Pan by double click again.",
    Move = "Move by double click within zoomed area.",
    Summarize = "Double click or select area to save data."
  )
)


#' @noRd
pick_hover <- function (data, hover, z) {
  if(is.null(hover)) return(data.frame())
  h <- round(unlist(hover[c("x", "y")], use.names = FALSE), 0L)
  data[data$x == h[1L] & data$y == h[2L], c("x", "y", z)]
}

#' @importFrom purrr map_if
#' @importFrom knitr kable
#' @noRd
format_hover <- function (h) {
  kable(
    cbind(
      paste0(names(h), ": "),
      unlist(map_if(h, is.double, ~ format(round(.x, 2L), nsmall = 2L)), use.names = FALSE)
    ),
    format="html", col.names = NULL, align = c("r", "r")
  )
}

#' @importFrom dplyr bind_rows everything mutate select summarize_if
#' @noRd
summarize_box <- function (data, box, .env, .format = format_summary) {
  .env$id <- .env$id + 1L
  .env$log <- data[
      box$xmin <= data$x & data$x <= box$xmax & 
      box$ymin <= data$y & data$y <= box$ymax, 
    ] %>>% 
    summarize_if(is.numeric, mean) %>>% 
    mutate(ID = !!.env$id, Area = "Box") %>>%
    select(ID, Area, x, y, everything()) %>>%
    bind_rows(.env$log)
  .format(.env$log)
}


#' @importFrom dplyr bind_rows everything mutate select
#' @noRd
summarize_click <- function (data, click, .env, .format = format_summary) {
  .env$id <- .env$id + 1L
  .env$log <- data[data$x == round(click$x) & data$y == round(click$y), ] %>>% 
    mutate(ID = !!.env$id, Area = "Click") %>>% 
    select(ID, Area, x, y, everything()) %>>% 
    bind_rows(.env$log)
  .format(.env$log)
}

#' @importFrom DT datatable formatRound
#' @noRd
format_summary <- function (summary) {
  formatRound(
    datatable(
      summary, 
      rownames = FALSE,
      options = list(
        scrollX = TRUE
      )
    ),
    setdiff(
      names(summary)[unlist(lapply(summary, is.numeric), use.names = FALSE)], 
      c("ID")
    )
  )
}

#' @importFrom shiny shinyApp
#' @noRd
plot_shiny <- function (x, y = setdiff(names(x), c('x', 'y'))[1L], pcol = TRUE, ...) {
  shinyApp(
    ui = ui(elm = setdiff(names(x), c("x", "y")), selected = y, pcol = TRUE),
    server = server(data = x)
  )  
}
