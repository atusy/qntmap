#' @importFrom shiny
#'  reactive reactiveVal reactiveValues
#' @importFrom shinyFiles
#'  getVolumes parseDirPath parseFilePaths shinyDirChoose shinyFileChoose
shiny_server <- function() {
  .margin <- c(-.5, .5)
  qnt_phase_list_csv <- tempfile()
  roots <- c("Working directory" = '.', getVolumes()())

  function(input, output, session) {
    
    # Input

    shiny_dir_choose(input, "xmap", roots = roots)
    update_path("xmap_dir_btn", "xmap_dir", input, session, roots)
    
    shiny_dir_choose(input, "qnt", roots = roots)
    update_path("qnt_dir_btn", "qnt_dir", input, session, roots)
    
    shiny_csv_choose(input, "phase_list_btn", roots = roots)
    update_path("phase_list_btn", "phase_list", input, session, roots, "file")
    
    xmap_data <- eventReactive(
      input$input_load,
      read_xmap(input$xmap_dir, DT = input$xmap_deadtime),
      ignoreNULL = FALSE
    )

    qnt_data <- eventReactive(
      input$input_load,
      read_qnt(
        input$qnt_dir, saving = FALSE, 
        phase_list = `if`(identical(input$phase_list, ""), NULL, input$phase_list)
      ),
      ignoreNULL = FALSE
    )

    output$xmap_cnd <- renderDT(dt(xmap_meta(xmap_data, input)))
    output$qnt_elm <- renderDT(dt(qnt_data()$elm))
    
    epma_data <- reactive(tidy_epma(qnt_data(), xmap_data()))
    step_size <- reactive(attr(xmap_data(), "step")[[1L]])
    
    # X-ray maps
    
    xmap_elint <- reactive(setdiff(names(xmap_data()), c("x", "y")))
    
    output$xmap_elem_selecter <- renderUI(select_elem("xmap", "Element", xmap_elint()))
    
    observeEvent(input$xmap_color, {
      shinyWidgets::updatePickerInput(session, "qmap_color", selected = input$xmap_color)
    })
    observeEvent(input$qmap_color, {
      shinyWidgets::updatePickerInput(session, "xmap_color", selected = input$qmap_color)
    })
    
    observeEvent(input$xmap_scale, {
      shinyWidgets::updatePickerInput(session, "qmap_scale", selected = input$xmap_scale)
      shinyWidgets::updatePickerInput(session, "cluster_scale", selected = input$xmap_scale)
    })
    observeEvent(input$cluster_scale, {
      shinyWidgets::updatePickerInput(session, "xmap_scale", selected = input$cluster_scale)
    })
    observeEvent(input$qmap_scale, {
      shinyWidgets::updatePickerInput(session, "xmap_scale", selected = input$qmap_scale)
    })
    
    
    ## X-ray mpas: action
    
    ranges <- reactiveValues()
    summary <- reactiveValues()
    observe_action("xmap", input, ranges, summary, xmap_data)
    
    ## X-ray maps: summary
    
    show_full_summary("xmap", input)

    output$xmap_summary <- renderDT(dt(summary$xmap, digits = 2L))
    output$xmap_summary_latest <- renderTable(
      summarize_latest(summary$xmap), align = "r"
    )
    
    ## X-ray maps: plot
    
    xmap_zlim <- zlim_react("xmap", xmap_data, input)
    xmap_squished <- squish_react("xmap", xmap_data, xmap_zlim, input)
    xmap_img <- reactive(as_img(
      lookup[[input$xmap_color]](xmap_squished(), from = xmap_zlim()),
      ranges$y0[2L], ranges$x0[2L]
    ))
    xmap_heatmap <- reactive(raster(
      xmap_img(), ranges, .margin, xmap_zlim(), input, "xmap", step_size()
    ))
    xmap_spot <- reactive(
      geom_point(
        aes(.data$y_px, .data$x_px), inherit.aes = FALSE, color = "green",
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
      picker_input(
        "qnt_y", label = NULL, choices = qnt_elem(), inline = FALSE,
        selected = qnt_elem()[[2L]]
      )
    })
    
    qnt_coords <- reactiveVal(NULL)
    observe_brush("qnt", input, qnt_coords)
    
    qnt_data_wt <- reactive(
      bind_cols(
        qnt_data()$cnd[c("id", "phase", "use")],
        qnt_data()$cmp$wt[c(input$qnt_x, input$qnt_y)]
      ) %>>%
        filter(.data$use)
    )
    
    output$qnt_plot <- renderPlot({
      req(qnt_data(), input$qnt_x, input$qnt_y)
      ggplot(qnt_data_wt(), aes(
        x = !!sym(input$qnt_x), y = !!sym(input$qnt_y),
        id = .data$id, color = .data$phase
      )) +
        scale_color_discrete(name = "Phase") +
        geom_point(size = 2) +
        theme_bw(base_size = 16) +
        qnt_coords()
    })
    
    qnt_plot_id <- reactiveVal("Here shows IDs of points withn 5 px from a click.")
    
    observeEvent(input$qnt_click_single, {
      pts <- shiny::nearPoints(qnt_data_wt(), input$qnt_click_single)
      req(nrow(pts) > 0L)
      qnt_plot_id(paste0("ID: ", paste(pts$id, collapse = ", ")))
    })

    output$qnt_plot_id <- renderPrint(cat(qnt_plot_id()))

    
    
    dt_qnt <- function(x, ...)  dt(prioritize(x, .component), ...)
    output$qnt_cnd <- renderDT(dt(qnt_data()$cnd))
    output$qnt_wt <- renderDT(dt_qnt(qnt_data()$cmp$wt))
    output$qnt_net <- renderDT(dt_qnt(qnt_data()$cmp$net))
    output$qnt_pkint <- renderDT(dt_qnt(qnt_data()$cmp$pkint))
    output$qnt_bgp <- renderDT(dt_qnt(qnt_data()$cmp$bgp))
    output$qnt_bgm <- renderDT(dt_qnt(qnt_data()$cmp$bgm))
    
    
    
    # Check
    
    outlier_elint <- reactive(intersect(xmap_elint(), qnt_elint()))
    output$outlier_elem_selecter <- renderUI(select_elem(
      "outlier", "Element to plot", outlier_elint()
    ))
    
    phase_all <- reactive(sort(unique(qnt_data()$cnd$phase)))
    output$outlier_phase <- renderUI(select_phase(phase_all()))
    
    outlier_coords <- reactiveVal(NULL)
    observe_brush("outlier", input, outlier_coords)

    outlier_plot_reactive <- reactive(
      outlier_gg(epma_data(), input, coords = outlier_coords())
    )
    
    output$outlier_plot <- renderPlot({
      req(epma_data(), input$outlier_elem)
      outlier_plot_reactive()
    })
    
    centroid <- reactive(find_centers(
      xmap_data(), qnt_data(), saveas = FALSE,
      phase = !!quo(setdiff(phase_all(), input$outlier_phase))
    ))
    
    output$centroid <- renderDT(dt(centroid(), digits = 2))
    
    
    
    # Cluster
    
    output$cluster_elint <- renderUI({
      choices <- xmap_elint()
      req(choices)
      shiny::checkboxGroupInput(
        "cluster_elint",
        "Elements to be used",
        choices = choices,
        selected = choices
      )
    })
    
    cluster_out <- eventReactive(input$cluster_run, {
      elements <- `if`(
        is.null(input$cluster_elint),
        intersect(names(xmap_data()), colnames(centroid())),
        input$cluster_elint
      )
      
      if (length(elements) < 2) stop("At least 2 elements must be chosen")
        
      cluster_xmap(xmap_data(), centroid(), saving = FALSE, elements = elements)
    })
    
    observe_action("cluster", input, ranges, summary, cluster_out)
    
    show_full_summary("cluster", input)
    
    cluster_z <- reactive(
      as.factor(
        if (input$cluster_subcluster == "Separated") {
          cluster_out()$cluster
        } else {
          gsub(input$cluster_suffix, "", cluster_out()$cluster)
        }
      )
    )
    
    cluster_zlim <- reactive(levels(cluster_z()))
    
    cluster_img <- reactive(
      as_img(lookup[["discrete"]](cluster_z()), ranges$y0[2L], ranges$x0[2L])
    )
    
    cluster_heatmap <- reactive(raster(
      cluster_img(), ranges, .margin, cluster_zlim(), input, "cluster", step_size()
    ))
    
    output$cluster_heatmap <- renderPlot({
      req(cluster_out())
      cluster_heatmap()
    })
    output$cluster_membership <- renderDT({
      req(cluster_out())
      dt(
        cluster_out(), digits = 2, 
        options = DT_options()[c("scrollY", "scrollCollapse")]
      )
    })
    output$cluster_centroid <- renderDT({
      req(cluster_out())
      dt(attr(cluster_out(), "center"), digits = 2)
    })
    output$cluster_summary <- renderDT({
      req(summary$cluster)
      dt(summary$cluster, digits = 2)
    })
    output$cluster_summary_latest <- renderTable(
      summarize_latest(summary$cluster),
      align = "r"
    )
    
    # Quantify
    
    qmap_out <- eventReactive(input$qmap_run, {
      req(cluster_out())
      quantify(
        xmap_data(), qnt_data(), cluster_out(), fine_phase = input$outlier,
        saving = FALSE
      )
    })

    qmap_elint <- reactive(setdiff(names(qmap_out()), c("x", "y")))
    
    output$qmap_elem_selecter <- renderUI(select_elem(
      "qmap", "Element",
      `if`(input$cluster_run == 0L || input$qmap_run == 0L, NULL, qmap_elint())
    ))
    
    qmap_density_df <- reactiveVal()
    
    shiny::observeEvent(phase_all(), {
      qmap_density_df(data.frame(phase = phase_all(), density = 1))
    })
    
    output$qmap_density <- renderDT(dt(
      qmap_density_df(), editable = list(target = "all"),
      options = DT_options(scrollY = "calc(100vh - 470px)")
    ))
    
    observeEvent(input$qmap_density_cell_edit, {
      qmap_density_df(
        suppressWarnings(editData(
          qmap_density_df(),
          filter(input$qmap_density_cell_edit, .data$col == 2), 
          "qmap_density"
        )) %>>%
          modify_at("density", as.numeric)
      )
    })
    
    qmap_density <- shiny::reactive({
      x <- setNames(qmap_density_df()$density, qmap_density_df()$phase)[cluster_out()$cluster]
      str(x)
      x
    })
    
    observe_action("qmap", input, ranges, summary, qmap_out, qmap_density)

    show_full_summary("qmap", input)
    
    output$qmap_summary <- renderDT({
      req(summary$qmap)
      dt(summary$qmap, digits = 2L)
    })
    output$qmap_summary_latest <- renderTable(
      summarize_latest(summary$qmap), align = "r"
    )


    qmap_zlim <- zlim_react("qmap", qmap_out, input)
    
    qmap_squished <- squish_react("qmap", qmap_out, qmap_zlim, input)
    
    qmap_img <- reactive(as_img(
      lookup[[input$qmap_color]](qmap_squished(), from = qmap_zlim()),
      ranges$y0[2L], ranges$x0[2L]
    ))
    
    qmap_heatmap <- reactive(raster(
      qmap_img(), ranges, .margin, qmap_zlim(), input, "qmap", step_size()
    ))
    output$qmap_heatmap <- renderPlot({
      req(qmap_out(), input$qmap_elem)
      qmap_heatmap()
    })
    
    qmap_histogram <- hist_react("qmap", qmap_out, input)
    output$qmap_histogram <- renderPlot(qmap_histogram())
    
    
    # Misc
    
    ## Params
    
    params <- reactive(retrieve_params(qmap_out()))
    
    output$params_elem_selecter <- renderUI(select_elem(
      "params", "Elements to plot", outlier_elint(), multiple = TRUE
    ))
    output$params_phase_selecter <- renderUI(picker_input(
      "params_phase", label = "Phases to plot", multiple = TRUE,
      choices = phase_all(), selected = phase_all()[1L]
    ))
    
    output$params_qmap <- DT::renderDT(params())
    autoplot_params <- function(type) {
      autoplot.tidy_epma(
        epma_data(), type = type, params = params(), origin = input$params_origin,
        element = input$params_elem, phase = input$params_phase
      )
    }
    output$params_alpha <- shiny::renderPlot(autoplot_params("alpha"))
    output$params_beta <- shiny::renderPlot(autoplot_params("beta"))
    output$params_gamma <- shiny::renderPlot(autoplot_params("gamma"))
    
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
  rownames = FALSE, digits = NULL, ...
) {
  datatable(
    data %>>% 
      round_if(digits = digits) %>%
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

round_if <- function(x, .p = is.double, digits = NULL) {
  if (is.null(digits)) return(x)
  modify_if(x, .p, round, digits)
}

select_elem <- function(id, label, choices, selected = choices[[1L]], ...) {
  picker_input(
    paste0(id, "_elem"),
    label = label,
    choices = choices,
    selected = selected,
    width = "100%",
    ...
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
    "Instrument", attr(xmap_data(), "instrument"), ""
  )
}

shiny_dir_choose <- function(
  input, id, roots = c("Working directory" = ".", getVolumes()())
) {
  shinyDirChoose(input, paste0(id, '_dir_btn'), root = roots, hidden = TRUE)
}

shiny_csv_choose <- function(
  input, id, roots = c("Working directory" = ".", getVolumes()())
) {
  shinyFileChoose(input, id, root = roots, filetypes = "csv", hidden = TRUE)
}

parse_path <- function(roots, selection, type = c("dir", "file")) {
  f <- list(dir = parseDirPath, file = parseFilePaths)[[type]]
  i <- list(dir = 1, file = "datapath")[[type]]
  f(roots, selection)[[i]]
}

update_path <- function(
  id_event, id_text, input, session, roots, type = "dir"
) {
  observeEvent(input[[id_event]], {
    req(is.list(input[[id_event]]))
    updateTextInput(
      session, id_text, label = NULL,
      value = parse_path(roots, input[[id_event]], type = type)
    )
  })
}

show_full_summary <- function(id, input) {
  observeEvent(input[[paste0(id, "_tab_summary")]], {
    shiny::showTab(paste0("main_tabset_", id), target = "Summary", select = TRUE)
  })
}

observe_brush <- function(id, input, reactive_value) {
  observeEvent(input[[paste0(id, "_click")]], {
    i <- input[[paste0(id, "_brush")]]
    reactive_value(
      coord_cartesian(xlim = c(i$xmin, i$xmax), ylim = c(i$ymin, i$ymax))
    )
  })
}