shiny_server <- function() {
  .margin <- c(-.5, .5)
  .env <- new.env()
  
  function(input, output, session) {
    
    # X-ray maps
    
    xmap_data <- shiny::reactive({
      input$xmap_read
      isolate(read_xmap(input$xmap_dir, DT = input$xmap_deadtime))
    })
    
    xmap_elint <- reactive(setdiff(names(xmap_data()), c("x", "y")))
    
    output$xmap_elem_selecter <- renderUI(select_elem("xmap", "Element", xmap_elint))

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
    
    # Spot
    
    qnt_data <- shiny::reactive({
      input$qnt_read
      isolate(read_qnt(input$qnt_dir, saving = FALSE))
    })
    
    qnt_elint <- reactive(qnt_data()$elm$elint)
    
    phase_list <- reactive(mutate(qnt_data()$cnd[c("id", "phase")], use = TRUE))
    output$qnt_phase_list <- renderDT(
      phase_list(),
      editable = list(target = "column", disable = list(columns = 0:1)),
      server = TRUE,
      caption = "Edit by double click a cell. Confirm by Ctrl + Enter",
      options = list(
        scrollX = TRUE, scrollY = "calc(100vh - 400px)",
        scrollCollapse = TRUE, paging = FALSE
      )
    )
    
    output$qnt_elm <- DT::renderDT(qnt_data()$elm, options = DT_options)
    output$qnt_cnd <- DT::renderDT(qnt_data()$cnd, options = DT_options)
    output$qnt_wt <- DT::renderDT(qnt_data()$cmp$wt, options = DT_options)
    output$qnt_net <- DT::renderDT(qnt_data()$cmp$net, options = DT_options)
    output$qnt_pkint <- DT::renderDT(qnt_data()$cmp$pkint, options = DT_options)
    output$qnt_bgp <- DT::renderDT(qnt_data()$cmp$bgp, options = DT_options)
    output$qnt_bgm <- DT::renderDT(qnt_data()$cmp$bgm, options = DT_options)
    
    
    
    # Check
    
    outlier_elint <- reactive(intersect(xmap_elint(), qnt_elint()))
    output$outlier_elem_selecter <- renderUI(select_elem(
      "outlier_elint", "Element to plot", outlier_elint
    ))
    output$outlier_phase <- renderUI(select_phase(qnt_data))
  }
}

DT_options <- list(
  scrollX = TRUE,
  scrollY = "calc(100vh - 300px)",
  scrollCollapse = TRUE,
  paging = FALSE
)

select_elem <- function(id, label, choices) {
  # selectInput(
  picker_input(
    paste0(id, "_elem"),
    label = label,
    choices = choices(),
    selected = choices()[[1L]],
    # selectize = FALSE,
    width = "100%"
  )
}

select_phase <- function(qnt_data) {
  picker_input(
    inputId = "outlier_phase",
    label = "Phases being outliers",
    choices = sort(unique(qnt_data()$cnd$phase)),
    multiple = TRUE
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