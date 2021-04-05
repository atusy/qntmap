#' @importFrom shiny
#'  reactive reactiveVal reactiveValues
#' @importFrom shinyFiles
#'  getVolumes parseDirPath parseFilePaths shinyDirChoose shinyFileChoose
shiny_server <- function(input, output, session) {
  
  # Global
  .margin <- c(-.5, .5)
  ranges <- shiny::reactiveValues()
  summary <- shiny::reactiveValues()
  as_img_ <- function(x) as_img(x, row = ranges$y0[2L], col = ranges$x0[2L])
  raster_ <- function(id, x, zlim, step_size) {
    raster(input, id, ranges, .margin, x = x, zlim = zlim, step_size = step_size)
  }
  
  # Input

  input_path(input, session, c("Working directory" = '.', getVolumes()()))

  xmap_data <- shiny::eventReactive(
    input$input_load,
    read_xmap(input$xmap_dir, DT = input$xmap_deadtime),
    ignoreNULL = FALSE
  )
  
  qnt_data <- shiny::reactiveVal(isolate(read_qnt(
    input$qnt_dir, saving = FALSE, 
    phase_list = `if`(input$phase_list != "", input$phase_list)
  )))
  
  shiny::observeEvent(input$input_load, {
    qnt_data(
      read_qnt(
        input$qnt_dir, saving = FALSE, 
        phase_list = `if`(input$phase_list != "", input$phase_list)
      )
    )
  })

  output$xmap_cnd <- DT::renderDT(DT(xmap_meta(xmap_data, input)))
  output$qnt_elm <- DT::renderDT(DT(qnt_data()$elm))
  
  xmap_elint <- shiny::reactive(setdiff(names(xmap_data()), c("x", "y")))
  epma_data <- shiny::reactive(tidy_epma(qnt_data(), xmap_data()))
  step_size <- shiny::reactive(attr(xmap_data(), "step")[[1L]])
  
  # X-ray maps
  
  output$xmap_elem_selecter <- shiny::renderUI(select_elem("xmap", "Element", xmap_elint()))
  
  observe_and_sync_color(input, session)
  observe_and_sync_scale(input, session)
  observe_action("xmap", input, ranges, summary, xmap_data)
  
  ## X-ray maps: summary
  
  show_full_summary("xmap", input)

  output$xmap_summary <- DT::renderDT(DT(summary$xmap, digits = 2L))
  output$xmap_summary_latest <- shiny::renderTable(
    summarize_latest(summary$xmap), align = "r"
  )
  
  ## X-ray maps: plot
  
  xmap_zlim <- shiny::reactive(zlim("xmap", xmap_data(), input))
  xmap_squished <- shiny::reactive(zquish("xmap", xmap_data(), xmap_zlim(), input))
  xmap_img <- shiny::reactive(as_img_(
    lookup[[input$xmap_color]](xmap_squished(), from = xmap_zlim())
  ))
  xmap_heatmap <- shiny::reactive(raster_(
    "xmap", xmap_img(), xmap_zlim(), step_size()
  ))
  xmap_spot <- shiny::reactive(
    if (input$xmap_show_spot) geom_point_qnt(epma_data())
  )
  
  output$xmap_heatmap <- shiny::renderPlot({
    shiny::req(input$xmap_elem)
    xmap_heatmap() + xmap_spot()
  })
  
  ## X-ray maps: histogram
  
  xmap_histogram <- hist_react("xmap", xmap_data, input)
  output$xmap_histogram <- shiny::renderPlot(xmap_histogram())
  
  # Spot
  
  qnt_elint <- shiny::reactive(qnt_data()$elm$elint)
  
  phase_list <- shiny::reactive(mutate(qnt_data()$cnd[c("phase", "use")]))
  
  output$qnt_phase_list <- DT::renderDT(DT(
    phase_list(), editable = list(target = "all"),
    options = DT_options(scrollY = "calc(100vh - 470px)")
  ))
  
  shiny::observeEvent(input$qnt_phase_list_cell_edit, {
    qnt_data(
      purrr::modify_at(qnt_data(), "cnd", function(.x) {
        dplyr::bind_cols(
          dplyr::mutate(.x, phase = NULL, use = NULL),
          # Suppress Warning in DT::coerceValue(v, data[i, j, drop = TRUE]) :
          #   The data type is not supported: logical
          suppressWarnings(DT::editData(
            phase_list(),
            dplyr::filter(input$qnt_phase_list_cell_edit, .data$col != 0), 
            "qnt_phase_list"
          )) %>>%
            purrr::modify_at("use", as.logical)
        )
      })
    )
  })
  
  qnt_elem <- shiny::reactive(prioritize(qnt_data()$elm$elem, .component))
  output$qnt_ui_x <- shiny::renderUI(
    picker_input("qnt_x", label = NULL, choices = qnt_elem(), inline = FALSE)
  )
  output$qnt_ui_y <- shiny::renderUI({
    picker_input(
      "qnt_y", label = NULL, choices = qnt_elem(), inline = FALSE,
      selected = qnt_elem()[[2L]]
    )
  })
  
  qnt_coords <- shiny::reactiveVal(NULL)
  observe_click_and_zoom("qnt", input, qnt_coords)
  
  qnt_data_wt <- shiny::reactive(
    dplyr::bind_cols(
      qnt_data()$cnd[c("id", "phase", "use")],
      qnt_data()$cmp$wt[c(input$qnt_x, input$qnt_y)]
    ) %>>%
      dplyr::filter(.data$use)
  )
  
  qnt_plot <- shiny::reactive({
    shiny::req(qnt_data(), input$qnt_x, input$qnt_y)
    autoplot_qnt(qnt_data_wt(), input$qnt_x, input$qnt_y, size = 2) +
      ggplot2::theme_bw(base_size = 16) +
      qnt_coords()
  })
  
  output$qnt_plot <- shiny::renderPlot(qnt_plot())

  qnt_plot_id <- shiny::reactiveVal("Here shows IDs of points withn 5 px from a click.")
  
  shiny::observeEvent(input$qnt_click_single, {
    pts <- shiny::nearPoints(qnt_data_wt(), input$qnt_click_single)
    shiny::req(nrow(pts) > 0L)
    qnt_plot_id(paste0("ID: ", paste(pts$id, collapse = ", ")))
  })

  output$qnt_plot_id <- shiny::renderPrint(cat(qnt_plot_id()))

  
  
  DT_qnt <- function(i, ...)  DT(prioritize(qnt_data()$cmp[[i]], .component), ...)
  output$qnt_cnd <- DT::renderDT(DT(qnt_data()$cnd))
  output$qnt_wt <- DT::renderDT(DT_qnt("wt"))
  output$qnt_net <- DT::renderDT(DT_qnt("net"))
  output$qnt_pkint <- DT::renderDT(DT_qnt("pkint"))
  output$qnt_bgp <- DT::renderDT(DT_qnt("bgp"))
  output$qnt_bgm <- DT::renderDT(DT_qnt("bgm"))
  
  
  
  # Check © 2019 JAMSTEC
  
  outlier_elint <- shiny::reactive(intersect(xmap_elint(), qnt_elint()))
  output$outlier_elem_selecter <- shiny::renderUI(select_elem(
    "outlier", "Element to plot", outlier_elint()
  ))
  
  phase_all <- shiny::reactive(sort(unique(phase_list()$phase[phase_list()$use])))
  output$outlier_phase <- shiny::renderUI(select_phase(phase_all()))
  
  outlier_coords <- shiny::reactiveVal(NULL)
  observe_click_and_zoom("outlier", input, outlier_coords)

  outlier_plot_reactive <- shiny::reactive(
    outlier_gg(epma_data(), input, coords = outlier_coords())
  )
  
  output$outlier_plot <- shiny::renderPlot({
    shiny::req(epma_data(), input$outlier_elem)
    outlier_plot_reactive()
  })
  
  centroid <- shiny::reactive(find_centers(
    xmap_data(), qnt_data(), saveas = FALSE,
    phase = !!rlang::quo(setdiff(phase_all(), input$outlier_phase))
  ))
  
  output$centroid <- DT::renderDT(DT(centroid(), digits = 2))
  
  
  
  # Cluster
  
  output$cluster_elint <- shiny::renderUI({
    choices <- xmap_elint()
    shiny::req(choices)
    shiny::checkboxGroupInput(
      "cluster_elint",
      "Elements to be used",
      choices = choices,
      selected = choices
    )
  })
  
  cluster_out <- shiny::eventReactive(input$cluster_run, {
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
  
  cluster_z <- shiny::reactive(
    as.factor(
      if (input$cluster_subcluster == "Separated") {
        cluster_out()$cluster
      } else {
        gsub(input$cluster_suffix, "", cluster_out()$cluster)
      }
    )
  )
  
  cluster_zlim <- shiny::reactive(levels(cluster_z()))
  
  cluster_img <- reactive(as_img_(lookup[["discrete"]](cluster_z())))
  
  cluster_heatmap <- shiny::reactive(raster_(
    "cluster", cluster_img(), cluster_zlim(), step_size()
  ))
  
  output$cluster_heatmap <- shiny::renderPlot(cluster_heatmap())
  output$cluster_membership <- DT::renderDT({
    shiny::req(cluster_out())
    DT(
      cluster_out(), digits = 2, 
      options = DT_options(paging = TRUE, pageLength = 100)
    )
  })
  output$cluster_centroid <- DT::renderDT({
    shiny::req(cluster_out())
    DT(attr(cluster_out(), "center"), digits = 2)
  })
  output$cluster_summary <- DT::renderDT({
    shiny::req(summary$cluster)
    DT(summary$cluster, digits = 2)
  })
  output$cluster_summary_latest <- shiny::renderTable(
    summarize_latest(summary$cluster), align = "r"
  )
  
  # Quantify
  
  qmap_out <- shiny::eventReactive(input$qmap_run, {
    shiny::req(cluster_out())
    quantify(
      xmap_data(), qnt_data(), cluster_out(),
      fine_phase = input$outlier_phase, fine_th = input$qmap_threshold,
      saving = FALSE
    )
  })
  
  output$qmap_dt <- DT::renderDT({
    shiny::req(qmap_out())
    DT(qmap_out(), digits = 2, options = DT_options(paging = TRUE, pageLength = 100))
  })

  qmap_elint <- shiny::reactive(setdiff(names(qmap_out()), c("x", "y")))
  
  output$qmap_elem_selecter <- shiny::renderUI(select_elem(
    "qmap", "Element",
    `if`(input$cluster_run == 0L || input$qmap_run == 0L, NULL, qmap_elint())
  ))
  
  qmap_density_df <- shiny::reactiveVal() # © 2019 JAMSTEC
  
  shiny::observeEvent(phase_all(), { # © 2019 JAMSTEC
    qmap_density_df(data.frame(phase = phase_all(), density = 1))
  })
  
  output$qmap_density <- DT::renderDT(DT( # © 2019 JAMSTEC
    qmap_density_df(), editable = list(target = "all"),
    options = DT_options(scrollY = "calc(100vh - 470px)")
  ))
  
  shiny::observeEvent(input$qmap_density_cell_edit, { # © 2019 JAMSTEC
    qmap_density_df(
      suppressWarnings(DT::editData(
        qmap_density_df(),
        dplyr::filter(input$qmap_density_cell_edit, .data$col == 2), 
        "qmap_density"
      )) %>>%
        purrr::modify_at("density", as.numeric)
    )
  })
  
  qmap_density <- shiny::reactive(
    setNames(qmap_density_df()$density, qmap_density_df()$phase)[cluster_out()$cluster]
  )
  
  observe_action("qmap", input, ranges, summary, qmap_out, qmap_density)

  show_full_summary("qmap", input)
  # show_tab(input, id, "summary", "Summary", "main")
  show_tab("qmap", input, "adv", "Adv.", "menu")
  
  output$qmap_summary <- DT::renderDT({
    shiny::req(summary$qmap)
    DT(summary$qmap, digits = 2L)
  })
  output$qmap_summary_latest <- shiny::renderTable(
    summarize_latest(summary$qmap), align = "r"
  )


  qmap_zlim <- reactive(zlim("qmap", qmap_out(), input))
  
  qmap_squished <- reactive(zquish("qmap", qmap_out(), qmap_zlim(), input))
  
  qmap_img <- shiny::reactive(as_img_(
    lookup[[input$qmap_color]](qmap_squished(), from = qmap_zlim())
  ))
  
  qmap_heatmap <- shiny::reactive(raster_(
    "qmap", qmap_img(), qmap_zlim(), step_size()
  ))
  output$qmap_heatmap <- shiny::renderPlot({
    shiny::req(qmap_out(), input$qmap_elem)
    qmap_heatmap()
  })
  
  qmap_histogram <- hist_react("qmap", qmap_out, input)
  output$qmap_histogram <- shiny::renderPlot(qmap_histogram())
  
  
  # Misc
  
  ## Params © 2019 JAMSTEC
  
  params <- shiny::reactive(retrieve_params(qmap_out(), xmap_data()))
  
  output$params_elem_selecter <- shiny::renderUI(select_elem(
    "params", "Elements to plot", outlier_elint(), multiple = TRUE
  ))
  output$params_phase_selecter <- shiny::renderUI(picker_input(
    "params_phase", label = "Phases to plot", multiple = TRUE,
    choices = phase_all(), selected = phase_all()[1L]
  ))
  
  output$params_qmap <- DT::renderDT(DT(params()))
  autoplot_params <- function(type) {
    autoplot.tidy_epma(
      epma_data(), type = type, params = params(), origin = input$params_origin,
      element = input$params_elem, phase = input$params_phase
    ) +
      ggplot2::theme_bw(base_size = 16)
  }
  output$params_alpha <- shiny::renderPlot(autoplot_params("alpha"))
  output$params_beta <- shiny::renderPlot(autoplot_params("beta"))
  output$params_gamma <- shiny::renderPlot(autoplot_params("gamma"))

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

DT <- function(
  data, options = DT_options(), filter = 'top', extensions = 'Buttons',
  rownames = FALSE, digits = NULL, ...
) {
  datatable(
    data %>>% 
      round_if(digits = digits) %>>%
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

attr_chr <- function(...) {
  as.character(attr(...))
}

xmap_meta <- function(xmap_data, input) {
  tribble(
    ~ Variables, ~ Values, ~ Units,
    "Elements", paste(setdiff(names(xmap_data()), c("x", "y")), collapse = ", "), "",
    "Dead time", attr_chr(xmap_data(), "deadtime"), "nsec",
    "Dwell", attr_chr(xmap_data(), "dwell"), "msec",
    "Probe current", attr_chr(xmap_data(), "current"), "A",
    "Start X", attr_chr(xmap_data(), "start")[[1L]], "mm",
    "Start Y", attr_chr(xmap_data(), "start")[[2L]], "mm",
    "Start Z", attr_chr(xmap_data(), "start")[[3L]], "mm",
    "Pixel size", attr_chr(xmap_data(), "pixel")[[1L]], "\u00b5m",
    "Step size", attr_chr(xmap_data(), "step")[[1L]], "\u00b5m",
    "Instrument", attr_chr(xmap_data(), "instrument"), ""
  )
}

shiny_dir_choose <- function(
  input, id, roots = c("Working directory" = ".", getVolumes()())
) {
  shinyFiles::shinyDirChoose(input, paste0(id, '_dir_btn'), root = roots, hidden = TRUE)
}

shiny_csv_choose <- function(
  input, id, roots = c("Working directory" = ".", getVolumes()())
) {
  shinyFiles::shinyFileChoose(input, id, root = roots, filetypes = "csv", hidden = TRUE)
}

input_path <- function(input, session, roots) {
  shiny_dir_choose(input, "xmap", roots = roots)
  update_path("xmap_dir_btn", "xmap_dir", input, session, roots)
  
  shiny_dir_choose(input, "qnt", roots = roots)
  update_path("qnt_dir_btn", "qnt_dir", input, session, roots)
  
  shiny_csv_choose(input, "phase_list_btn", roots = roots)
  update_path("phase_list_btn", "phase_list", input, session, roots, "file")
}

parse_path <- function(roots, selection, type = c("dir", "file")) {
  f <- list(dir = parseDirPath, file = parseFilePaths)[[type]]
  i <- list(dir = 1, file = "datapath")[[type]]
  f(roots, selection)[[i]]
}

update_path <- function(
  id_event, id_text, input, session, roots, type = "dir"
) {
  shiny::observeEvent(input[[id_event]], {
    shiny::req(is.list(input[[id_event]]))
    shiny::updateTextInput(
      session, id_text, label = NULL,
      value = parse_path(roots, input[[id_event]], type = type)
    )
  })
}

show_tab <- function(id, input, tab, target, pane = "main") {
  shiny::observeEvent(input[[paste0(id, "_tab_", tab)]], {
    shiny::showTab(paste0(pane, "_tabset_", id), target = target, select = TRUE)
  })
}

show_full_summary <- function(id, input) {
  show_tab(id, input, "summary", "Summary", "main")
}

observe_click_and_zoom <- function(id, input, reactive_value) {
  shiny::observeEvent(input[[paste0(id, "_click")]], {
    i <- input[[paste0(id, "_brush")]]
    reactive_value(
      ggplot2::coord_cartesian(xlim = c(i$xmin, i$xmax), ylim = c(i$ymin, i$ymax))
    )
  })
}
