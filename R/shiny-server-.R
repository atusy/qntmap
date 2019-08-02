#' @importFrom shiny
#' reactive reactiveVal reactiveValues

shiny_server <- function() {
  .margin <- c(-.5, .5)
  qnt_phase_list_csv <- tempfile()

  function(input, output, session) {
    
    # Input

    xmap_data <- reactiveVal()
    qnt_data <- reactiveVal()
    isolate({
      xmap_data(read_xmap(input$xmap_dir, DT = input$xmap_deadtime))
      qnt_data(read_qnt(
        input$qnt_dir, saving = FALSE, 
        phase_list = `if`(identical(input$phase_list, ""), NULL, input$phase_list)
      ))
    })
    observeEvent(input$input_load, {
      xmap_data(read_xmap(input$xmap_dir, DT = input$xmap_deadtime))
      qnt_data(read_qnt(
        input$qnt_dir, saving = FALSE, 
        phase_list = `if`(identical(input$phase_list, ""), NULL, input$phase_list)
      ))
    })

    output$xmap_meta <- renderDT(dt(xmap_meta(xmap_data, input), options = DT_options()))
    
    epma_data <- reactive(tidy_epma(qnt_data(), xmap_data()))
    
    # X-ray maps
    
    xmap_elint <- reactive(setdiff(names(xmap_data()), c("x", "y")))
    
    output$xmap_elem_selecter <- renderUI(select_elem("xmap", "Element", xmap_elint))
    
    ## X-ray mpas: action
    
    range_x <- reactive(range(xmap_data()$x))
    range_y <- reactive(range(xmap_data()$y))
    ranges <- reactiveValues()
    summary <- reactiveValues()
    observe_action("xmap", input, ranges, range_x, range_y, summary, xmap_data)
    
    ## X-ray maps: summary
    
    show_full_summary <- function(id, input) {
      observeEvent(input[[paste0(id, "_tab_summary")]], {
        shiny::showTab(paste0("main_tabset_", id), target = "Summary", select = TRUE)
      })
    }
    show_full_summary("xmap", input)

    output$xmap_summary <- renderDT(dt(
      modify_if(summary$xmap, is.double, round, 2L)
    ))
    output$xmap_summary_latest <- renderTable(
      summarize_latest(summary$xmap), align = "r"
    )
    
    ## X-ray maps: plot
    
    xmap_zlim <- zlim_react("xmap", xmap_data, input)
    xmap_squished <- squish_react("xmap", xmap_data, xmap_zlim, input)
    xmap_img <- reactive(as_img(
      lookup[[input$xmap_color]](xmap_squished(), from = xmap_zlim()),
      range_y()[2L], range_x()[2L]
    ))
    xmap_heatmap <- raster_react(
      xmap_img, ranges, range_x, range_y, .margin, xmap_zlim, input, "xmap"
    )
    xmap_spot <- reactive(
      geom_point(
        aes(.data$y_px, .data$x_px), inherit.aes = FALSE, color = "red",
        filter(epma_data(), .data$elint == .data$elint[[1L]])[c("x_px", "y_px")]
      )
    )
    
    output$xmap_heatmap <- renderPlot({
      req(input$xmap_elem)
      xmap_heatmap() + `if`(isTRUE(input$xmap_show_spot), xmap_spot())
    })
    
    ## X-ray maps: histogram
    
    xmap_histogram <- hist_react("xmap", xmap_data, input)
    output$xmap_histogram <- renderPlot(xmap_histogram())
    
    # Spot
    
    qnt_elint <- reactive(qnt_data()$elm$elint)
    
    phase_list <- reactive(mutate(qnt_data()$cnd[c("phase", "use")]))
    
    output$qnt_phase_list <- renderDT(
      dt(
        phase_list(), editable = list(target = "all"),
        options = DT_options(scrollY = "calc(100vh - 470px)")
      )
    )
    
    observeEvent(input$qnt_phase_list_cell_edit, {
      qnt_data(
        modify_at(qnt_data(), "cnd", function(.x) {
          bind_cols(
            mutate(.x, phase = NULL, use = NULL),
            # Suppress Warning in DT::coerceValue(v, data[i, j, drop = TRUE]) :
            #   The data type is not supported: logical
            suppressWarnings(editData(
              phase_list(),
              filter(input$qnt_phase_list_cell_edit, .data$col != 0), 
              "qnt_phase_list"
            )) %>>%
              modify_at("use", as.logical)
          )
        })
      )
    })
    
    qnt_elem <- reactive(prioritize(qnt_data()$elm$elem, .component))
    output$qnt_ui_x <- renderUI(
      picker_input(
        "qnt_x", label = NULL, choices = qnt_elem(), inline = FALSE,
        selected = qnt_elem()[[1L]]
      )
    )
    output$qnt_ui_y <- renderUI({
      choices <- setdiff(qnt_elem(), input$qnt_x)
      picker_input(
        "qnt_y", label = NULL, choices = qnt_elem(), inline = FALSE,
        selected = qnt_elem()[[2L]]
      )
    })
    
    output$qnt_plot <- plotly::renderPlotly({
      req(qnt_data(), input$qnt_x, input$qnt_y)
      ggplotly2(
        bind_cols(
          qnt_data()$cnd[c("id", "phase", "use")],
          qnt_data()$cmp$wt[c(input$qnt_x, input$qnt_y)]
        ) %>>%
          filter(.data$use) %>>%
          ggplot(aes(
            x = !!sym(input$qnt_x), y = !!sym(input$qnt_y),
            id = !!quo(id), color = !!quo(phase)
          )) +
          geom_point() +
          theme_bw(base_size = 16)
      )
    })
    output$qnt_elm <- renderDT(dt(qnt_data()$elm))
    output$qnt_cnd <- renderDT(dt(qnt_data()$cnd))
    output$qnt_wt <- renderDT(dt(qnt_data()$cmp$wt))
    output$qnt_net <- renderDT(dt(qnt_data()$cmp$net))
    output$qnt_pkint <- renderDT(dt(qnt_data()$cmp$pkint))
    output$qnt_bgp <- renderDT(dt(qnt_data()$cmp$bgp))
    output$qnt_bgm <- renderDT(dt(qnt_data()$cmp$bgm))
    
    
    
    # Check
    
    outlier_elint <- reactive(intersect(xmap_elint(), qnt_elint()))
    output$outlier_elem_selecter <- renderUI(select_elem(
      "outlier", "Element to plot", outlier_elint
    ))
    outlier_phase_all <- reactive(sort(unique(qnt_data()$cnd$phase)))
    output$outlier_phase <- renderUI(select_phase(outlier_phase_all()))
    outlier_plot_reactive <- outlier_gg_react(epma_data, input)
    output$outlier_plot <- renderPlot(outlier_plot_reactive())

    centroid <- reactive(find_centers(
      xmap_data(), qnt_data(), saveas = FALSE,
      phase = !!quo(setdiff(outlier_phase_all(), input$outlier_phase))
    ))
    
    output$centroid <- renderDT(dt(modify_if(centroid(), is.double, round, 2)))
    
    
    
    # Cluster
    
    cluster_out <- reactiveVal()
    
    observe_action("cluster", input, ranges, range_x, range_y, summary, cluster_out)
    
    show_full_summary("cluster", input)
    
    observeEvent(input$cluster_run, {
      cluster_out(cluster_xmap(xmap_data(), centroid()))
    })
    
    cluster_z <- reactive({
      req(cluster_out())
      as.factor(
        if (input$cluster_subcluster == "Asis") {
          cluster_out()$cluster
        } else {
          gsub(input$cluster_suffix, "", cluster_out()$cluster)
        }
      )
    })
    
    cluster_zlim <- reactive({
      req(cluster_out())
      levels(cluster_z())
    })
    
    cluster_img <- reactive({
      req(cluster_out())
      as_img(lookup[["discrete"]](cluster_z()), range_y()[2L], range_x()[2L])
    })
    
    cluster_heatmap <- raster_react(
      cluster_img, ranges, range_x, range_y, .margin, cluster_zlim, input, "cluster"
    )
    
    output$cluster_heatmap <- renderPlot(cluster_heatmap())
    output$cluster_membership <- renderDT({
      req(cluster_out())
      dt(
        modify_if(cluster_out(), is.double, round, 2), 
        options = DT_options()[c("scrollY", "scrollCollapse")]
      )
    })
    output$cluster_centroid <- renderDT({
      req(cluster_out())
      dt(modify_if(attr(cluster_out(), "center"), is.double, round, 2))
    })
    output$cluster_summary <- renderDT({
      req(summary$cluster)
      dt(modify_if(summary$cluster, is.double, round, 2))
    })
    output$cluster_summary_latest <- renderTable(
      summarize_latest(summary$cluster),
      align = "r"
    )
    
    # Quantify
    
    qmap_out <- reactiveVal()

    qmap_elint <- reactive(setdiff(names(qmap_out()), c("x", "y")))
    
    output$qmap_elem_selecter <- renderUI(select_elem("qmap", "Element", qmap_elint))
    
    observeEvent(input$qmap_run, {
      req(cluster_out())
      qmap_out(quantify(xmap_data(), qnt_data(), cluster_out(), fine_phase = input$outlier))
    })
    
    observe_action("qmap", input, ranges, range_x, range_y, summary, cluster_out)

    show_full_summary("qmap", input)
    
    output$qmap_summary <- renderDT({
      req(summary$qmap)
      dt(modify_if(summary$qmap, is.double, round, 2L))
    })
    output$qmap_summary_latest <- renderTable(
      summarize_latest(summary$qmap), align = "r"
    )


    qmap_zlim <- zlim_react("qmap", qmap_out, input)
    
    qmap_squished <- squish_react("qmap", qmap_out, qmap_zlim, input)
    
    qmap_img <- reactive(as_img(
      lookup[[input$qmap_color]](qmap_squished(), from = qmap_zlim()),
      range_y()[2L], range_x()[2L]
    ))
    
    qmap_heatmap <- raster_react(
      qmap_img, ranges, range_x, range_y, .margin, qmap_zlim, input, "qmap"
    )
    output$qmap_heatmap <- renderPlot({
      req(qmap_out())
      qmap_heatmap()
    })
    
    qmap_histogram <- hist_react("qmap", qmap_out, input)
    output$qmap_histogram <- renderPlot(qmap_histogram())
    
  }
}

DT_options <- function(
  ...,
  scrollX = TRUE, scrollY = "calc(100vh - 330px)", scrollCollapse = TRUE,
  paging = FALSE, searching = TRUE, dom = "ftB", buttons = c("csv", "excel")
) {
  list(
    scrollX = scrollX,
    scrollY = scrollY,
    scrollCollapse = scrollCollapse,
    paging = paging,
    searching = searching,
    dom = dom,
    buttons = buttons,
    ...
  )
}

dt <- function(
  data, options = DT_options(), filter = 'top', extensions = 'Buttons',
  rownames = FALSE, ...
) {
  datatable(
    data %>>% 
      mutate(n = row_number()) %>>% 
      select("n", everything()) %>>%
      setNames(gsub("^n$", "", names(.))),
    options = options,
    filter = filter,
    extensions = extensions,
    rownames = rownames,
    ...
  )
}

select_elem <- function(id, label, choices) {
  picker_input(
    paste0(id, "_elem"),
    label = label,
    choices = choices(),
    selected = choices()[[1L]],
    width = "100%"
  )
}

select_phase <- function(phase) {
  picker_input(
    inputId = "outlier_phase",
    label = "Phases being outliers",
    choices = phase,
    multiple = TRUE
  )
}

xmap_meta <- function(xmap_data, input) {
  tribble(
    ~ Variables, ~ Values, ~ Units,
    "Elements", paste(setdiff(names(xmap_data()), c("x", "y")), collapse = ", "), "",
    "Dead time", attr(xmap_data(), "deadtime"), "nsec",
    "Dwell", attr(xmap_data(), "dwell"), "msec",
    "Probe current", attr(xmap_data(), "current"), "A",
    "Start X", attr(xmap_data(), "start")[[1L]], "mm",
    "Start Y", attr(xmap_data(), "start")[[2L]], "mm",
    "Start Z", attr(xmap_data(), "start")[[3L]], "mm",
    "Pixel size", attr(xmap_data(), "pixel")[[1L]], "\u00b5m",
    "Step size", attr(xmap_data(), "step")[[1L]], "\u00b5m",
    "Instrument", attr(xmap_data(), "instrument"), "",
    "Path", input$xmap_dir, ""
  )
}

ggplotly2 <- function(p, dynamicTicks = TRUE, ...) {
  plotly::ggplotly(p, dynamicTicks = dynamicTicks, ...) %>>%
    plotly::config(
      displaylogo = FALSE,
      modeBarButtonsToRemove = list(
        'lasso2d',
        'select2d', # box select
        'sendDataToCloud',
        'toImage',
        'autoScale2d',
        'hoverClosestCartesian',
        'hoverCompareCartesian',
        'zoomOut2d',
        'zoomIn2d'
      )
    )
}