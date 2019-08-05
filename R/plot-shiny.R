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
ui <- function(elm, selected = elm[[1L]], pcol = TRUE) {
  fluidPage(sidebarLayout(

    sidebarPanel(
      splitLayout(
        selectInput("fill", "Element", elm, selected = selected, selectize = FALSE),
        numericInput("min", "Min", value = NA_real_),
        numericInput("max", "Max", value = NA_real_)
      ),
      plotOutput("hist"), br(),
      splitLayout(
        "Height of graph [px]", numericInput("height", NULL, value = 600L)
      ),
      checkboxInput("pcol", "Pseudocolor", value = pcol)
    ),

    mainPanel(
      # shiny::verbatimTextOutput("debug"),
      tags$head(tags$style("#tip {position: absolute; width: auto; z-index: 100;}")),
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
        height = "auto"
      ),
      uiOutput("tip"),
      dataTableOutput("dt")
    )
  ))
}











#' @importFrom DT renderDT
#' @importFrom scales squish
#' @importFrom shiny
#'   htmlOutput reactive reactiveValues observeEvent
#'   renderPlot renderPrint renderUI req
#' @noRd
server <- function(data) {
  .margin <- c(-.5, .5)
  range_x <- range(data$x)
  range_y <- range(data$y)

  .env <- new.env()
  .env$log <- data %>>%
    summarize_if(is.numeric, mean) %>>%
    mutate(ID = 0L, Area = "Whole") %>>%
    select("ID", "Area", "x", "y", everything())
  .env$id <- 0L

  function(input, output) {

    z_is_num <- reactive(is.numeric(data[[input$fill]]))
    colors <- reactive(`if`(z_is_num(), `if`(input$pcol, "viridis", "gray"), "discrete"))

    output$hist <- renderPlot(gghist(
      data[[input$fill]], input[["min"]], input[["max"]], colors = colors()
    ))

    output$mouseHelp <- renderPrint(cat(msg$mouseHelp[input$mouse]))

    ranges <- reactiveValues(x = range_x, y = range_y)

    observeEvent(input$click, {
      if (input$mouse == "Zoom")
        if (!is.null(input$brush)) {
          ranges$x <- squish(c(input$brush$xmin, input$brush$xmax), range_x)
          ranges$y <- squish(c(input$brush$ymin, input$brush$ymax), range_y)
        } else {
          ranges$x <- range_x
          ranges$y <- range_y
        }
      if (input$mouse == "Move")
        if (!is.null(ranges$x)) {
          temp <- range_x + c(1, -1) * (ranges$x[2] - ranges$x[1]) / 2
          ranges$x <- ranges$x + squish(input$click$x, temp) - mean(ranges$x)
          temp <- range_y + c(1, -1) * (ranges$y[2] - ranges$y[1]) / 2
          ranges$y <- ranges$y + squish(input$click$y, temp) - mean(ranges$y)
        }
    })

    # Reacts to input$min, input$max, inputfill
    zlim <- reactive(if (z_is_num()) {
      zrange <- range(data[[input$fill]])
      c(
        `if`(is.na(input$min) || input$min < zrange[1], zrange[1], input$min),
        `if`(is.na(input$max) || input$max > zrange[2], zrange[2], input$max)
      )
    } else {
      levels(data[[input$fill]])
    })

    # Reacts to zlim(), input$fill
    squished <- reactive(
      `if`(z_is_num(), squish(data[[input$fill]], zlim()), data[[input$fill]])
    )

    # Reacts to colors(), squished()
    # suqished() reacts to zlim()
    img <- reactive(as_img(
      lookup[[colors()]](squished(), from = zlim()), range_y[2], range_x[2]
    ))

    # React to colors(), img(), ranges$y, rangex$x
    # img() reacts to zlim()
    output$heatmap <- renderPlot(
      gg_img(
        img()[ranges$y[1]:ranges$y[2], ranges$x[1]:ranges$x[2], ],
        xlim = ranges$x + .margin, ylim = ranges$y + .margin,
        zlim = zlim(), zname = input$fill, colors = colors()
      ),
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
      if (input$mouse != "Summarize")
        return(format_summary(.env$log))
      if (!is.null(input$brush))
        return(summarize_box(data, input$brush, .env))
      if (!is.null(input$click))
        return(summarize_click(data, input$click, .env))
      format_summary(.env$log)
    })

    output$dt <- renderDT(dt())

    # output$debug <- shiny::renderText(zlim())
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
pick_hover <- function(data, hover, z) {
  if (is.null(hover)) return(data.frame())
  h <- round(unlist(hover[c("x", "y")], use.names = FALSE), 0L)
  data[data$x == h[1L] & data$y == h[2L], c("x", "y", z)]
}

#' @importFrom knitr kable
#' @noRd
format_hover <- function(h) {
  kable(
    cbind(
      paste0(names(h), ": "),
      unlist(map_if(h, is.double, ~ format(round(.x, 2L), nsmall = 2L)), use.names = FALSE)
    ),
    format = "html", col.names = NULL, align = c("r", "r")
  )
}

#' @noRd
summarize_box <- function(data, box, .env, .format = format_summary) {
  .env$id <- .env$id + 1L
  .env$log <- data[
    box$xmin <= data$x & data$x <= box$xmax &
      box$ymin <= data$y & data$y <= box$ymax,
  ] %>>%
    summarize_if(is.numeric, mean) %>>%
    mutate(ID = !!.env$id, Area = "Box") %>>%
    select("ID", "Area", "x", "y", everything()) %>>%
    bind_rows(.env$log)
  .format(.env$log)
}


#' @noRd
summarize_click <- function(data, click, .env, .format = format_summary) {
  .env$id <- .env$id + 1L
  .env$log <- data[data$x == round(click$x) & data$y == round(click$y), ] %>>%
    mutate(ID = !!.env$id, Area = "Click") %>>%
    select("ID", "Area", "x", "y", everything()) %>>%
    bind_rows(.env$log)
  .format(.env$log)
}

#' @importFrom DT datatable formatRound
#' @noRd
format_summary <- function(summary) {
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
plot_shiny <- function(x, y = setdiff(names(x), c("x", "y"))[1L], pcol = TRUE, ...) {
  shinyApp(
    ui = ui(elm = setdiff(names(x), c("x", "y")), selected = y, pcol = TRUE),
    server = server(data = x)
  )
}
