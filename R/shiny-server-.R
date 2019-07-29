#' @importFrom shiny
#' reactive reactiveVal reactiveValues

shiny_server <- function(phase_list = NULL) {
  .margin <- c(-.5, .5)
  qnt_phase_list_csv0 <- phase_list
  qnt_phase_list_csv <- tempfile()

  function(input, output, session) {
    
    # Input
    
    xmap_data <- reactive({
      input$reload
      isolate(read_xmap(input$xmap_dir, DT = input$xmap_deadtime))
    })
    
    qnt_phase_list_mod <- reactiveVal()
    qnt_data <- reactive({
      input$reload
      input$qnt_phase_list_confirm
      isolate({
        mod <- !is.null(qnt_phase_list_mod())
        if (mod) fwrite(qnt_phase_list_mod(), qnt_phase_list_csv)
        read_qnt(
          input$qnt_dir, saving = FALSE, 
          phase_list = if (mod) qnt_phase_list_csv else qnt_phase_list_csv0
        )
      })
    })
    
    epma_data <- reactive(tidy_epma(qnt_data(), xmap_data()))
    
    # X-ray maps
    
    xmap_elint <- reactive(setdiff(names(xmap_data()), c("x", "y")))
    
    output$xmap_elem_selecter <- renderUI(select_elem("xmap", "Element", xmap_elint))
    
    output$xmap_meta <- DT::renderDT(dt(xmap_meta(xmap_data, input), options = DT_options))
    
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
    
    output$xmap_summary <- DT::renderDT(dt(
      modify_if(summary$xmap, is.double, round, 2L)
    ))
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
    xmap_heatmap <- raster_react(
      xmap_img, ranges, range_x, range_y, .margin, xmap_zlim, input, "xmap"
    )
    xmap_spot <- reactive(
      geom_point(
        aes(.data$y_px, .data$x_px), inherit.aes = FALSE, color = "red",
        epma_data() %>>%
          filter(.data$elint == .data$elint[[1L]]) %>>%
          select("x_px", "y_px")
      )
    )
    
    output$xmap_heatmap <- renderPlot(
      xmap_heatmap() + `if`(isTRUE(input$xmap_show_spot), xmap_spot())
    )
    
    ## X-ray maps: histogram
    
    xmap_histogram <- hist_react("xmap", xmap_data, input)
    output$xmap_histogram <- renderPlot(xmap_histogram())
    
    # Spot
    
    shiny::observeEvent(input$qnt_phase_list_cell_edit, {
      qnt_phase_list_mod(
        DT::editData(
          phase_list(), input$qnt_phase_list_cell_edit, "qnt_phase_list",
        )
      )
    })
    
    qnt_elint <- reactive(qnt_data()$elm$elint)
    
    phase_list <- reactive(mutate(qnt_data()$cnd[c("id", "phase")], use = TRUE))
    
    output$qnt_phase_list <- renderDT(
      phase_list(),
      editable = list(target = "column", disable = list(columns = 0:1)),
      server = TRUE,
      options = list(
        scrollX = TRUE, scrollY = "calc(100vh - 400px)",
        scrollCollapse = TRUE, paging = FALSE
      )
    )
    
    output$qnt_elm <- DT::renderDT(dt(qnt_data()$elm))
    output$qnt_cnd <- DT::renderDT(dt(qnt_data()$cnd))
    output$qnt_wt <- DT::renderDT(dt(qnt_data()$cmp$wt))
    output$qnt_net <- DT::renderDT(dt(qnt_data()$cmp$net))
    output$qnt_pkint <- DT::renderDT(dt(qnt_data()$cmp$pkint))
    output$qnt_bgp <- DT::renderDT(dt(qnt_data()$cmp$bgp))
    output$qnt_bgm <- DT::renderDT(dt(qnt_data()$cmp$bgm))
    
    
    
    # Check
    
    outlier_elint <- reactive(intersect(xmap_elint(), qnt_elint()))
    output$outlier_elem_selecter <- renderUI(select_elem(
      "outlier", "Element to plot", outlier_elint
    ))
    output$outlier_phase <- renderUI(select_phase(qnt_data))
    outlier_plot_reactive <- outlier_gg_react(epma_data, input)
    output$outlier_plot <- renderPlot(outlier_plot_reactive())
    
    centroid <- reactive(find_centers(
      xmap_data(), qnt_data(), fine_phase = input$outlier_phase, saveas = FALSE
    ))
    
    output$centroid <- DT::renderDT(dt(
      mutate_if(centroid(), is.double, round, 2)
    ))
    
    
    
    # Cluster
    
    cluster_out <- shiny::reactiveVal()
    
    observe_action("cluster", input, ranges, range_x, range_y, summary, cluster_out)
    
    shiny::observeEvent(input$cluster_run, {
      cluster_out(cluster_xmap(xmap_data(), centroid()))
    })
    
    cluster_z <- reactive({
      req(cluster_out())
      as.factor(
        if (input$cluster_subcluster == "Asis") {
          cluster_out()$cluster
        } else {
          gsub(input$cluster_subcluster, "", cluster_out()$cluster)
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
    output$cluster_membership <- DT::renderDT({
      req(cluster_out())
      dt(
        modify_if(cluster_out(), is.double, round, 2), 
        options = DT_options[c("scrollY", "scrollCollapse")]
      )
    })
    output$cluster_centroid <- DT::renderDT({
      req(cluster_out())
      dt(modify_if(attr(cluster_out(), "center"), is.double, round, 2))
    })
    output$cluster_summary <- DT::renderDT({
      req(summary$cluster)
      dt(modify_if(summary$cluster, is.double, round, 2))
    })
    output$cluster_summary_latest <- shiny::renderTable(
      summarize_latest(summary$cluster),
      align = "r"
    )
    
    # Quantify
    
    qmap_out <- shiny::reactiveVal()

    qmap_elint <- reactive(setdiff(names(qmap_out()), c("x", "y")))
    
    output$qmap_elem_selecter <- renderUI(select_elem("qmap", "Element", qmap_elint))
    
    observeEvent(input$qmap_run, {
      req(cluster_out())
      qmap_out(quantify(xmap_data(), qnt_data(), cluster_out(), fine_phase = input$outlier))
    })
    
    observe_action("qmap", input, ranges, range_x, range_y, summary, cluster_out)

    output$qmap_summary <- DT::renderDT({
      req(summary$qmap)
      dt(modify_if(summary$qmap, is.double, round, 2L))
    })
    output$qmap_summary_latest <- shiny::renderTable(
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
    
    output$test <- renderPrint(str(qmap_out()))
    
  }
}

DT_options <- list(
  scrollX = TRUE,
  scrollY = "calc(100vh - 300px)",
  scrollCollapse = TRUE,
  paging = FALSE,
  columnDefs = list(
    list(orderable = TRUE, targets = 0)
  )
)

dt <- function(data, options = DT_options, ...) {
  DT::datatable(
    data %>>% 
      mutate(n = row_number()) %>>% 
      select("n", everything()) %>>%
      setNames(gsub("^n$", "", names(.))),
    options = options,
    rownames = FALSE
  )
}

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
