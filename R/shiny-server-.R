shiny_server <- function() {
  .margin <- c(-.5, .5)
  .env <- new.env()
  
  function(input, output, session) {
    
    # X-ray maps
    
    xmap_data <- shiny::reactive({
      input$xmap_read
      isolate(read_xmap(input$xmap_dir, DT = input$xmap_deadtime))
    })
    
    output$xmap_elem_selecter <- renderUI(select_elem("xmap", "Element", xmap_data))

    output$xmap_meta <- DT::renderDT(
      xmap_meta(xmap_data, input), options = list(pageLength = 11L)
    )
    
    ## X-ray mpas: action
    
    range_x <- reactive(range(xmap_data()$x))
    range_y <- reactive(range(xmap_data()$y))
    ranges <- shiny::reactiveValues()
    summary <- shiny::reactiveValues()
    observe_action("xmap", input, ranges, range_x, range_y, summary, xmap_data)

    output$xmap_message_action <- shiny::renderText(
      message_action[[input$xmap_action]]
    )

    ## X-ray maps: summary
    
    output$xmap_summary <- DT::renderDT(summary$xmap)
    output$xmap_summary_latest <- shiny::renderTable(
      summarize_latest(summary$xmap), align = "r"
    )
    
    ## X-ray maps: plot
    
    xmap_zlim <- zlim_react("xmap", xmap_data, input)
    xmap_squished <- squish_react("xmap", xmap_data, xmap_zlim, input)
    xmap_img <- reactive(qntmap:::as_img(
      lookup[[input$xmap_color]](xmap_squished(), from = xmap_zlim()),
      range_y()[2L], range_x()[2L]
    ))
    xmap_heatmap <- raster_react(xmap_img, ranges, range_x, range_y, .margin, xmap_zlim, input, "xmap")
    output$xmap_heatmap <- renderPlot(xmap_heatmap())
    
    ## X-ray maps: histogram
    
    xmap_histogram <- hist_react("xmap", xmap_data, input)
    output$xmap_histogram <- renderPlot(xmap_histogram())
  }
}

select_elem <- function(id, label, data) {
  choices <- setdiff(names(data()), c("x", "y"))
  selectInput(
    paste0(id, "_elem"),
    label = label,
    choices = choices,
    selected = choices[[1L]],
    width = "100%",
    selectize = FALSE
  )
}

xmap_meta <- function(xmap_data, input) {
  tibble::tribble(
    ~ Varables, ~ Values, ~ Units,
    "Elements", paste(setdiff(names(xmap_data()), c("x", "y")), collapse = ", "), "",
    "Dead time", attr(xmap_data(), "deadtime"), "nsec",
    "Dwell", attr(xmap_data(), "dwell"), "msec",
    "Probe current", attr(xmap_data(), "current"), "A",
    "Start X", attr(xmap_data(), "start")[[1L]], "mm",
    "Start Y", attr(xmap_data(), "start")[[2L]], "mm",
    "Start Z", attr(xmap_data(), "start")[[3L]], "mm",
    "Pixel size", attr(xmap_data(), "pixel")[[1L]], "μm",
    "Step size", attr(xmap_data(), "step")[[1L]], "μm",
    "Instrument", attr(xmap_data(), "instrument"), "",
    "Path", input$xmap_dir, ""
  )
}
