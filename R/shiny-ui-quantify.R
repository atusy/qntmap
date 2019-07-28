shiny_ui_quantify <- function() {
  shiny::tabPanel(
    "Quantify",
    shiny::sidebarLayout(
      sidebar_tabset_panel(
        # Visual
        tab_panel(
          "Visual",
          shiny::actionButton(
            "qmap_run", label = tags$strong("Quantify Maps"), width = "100%",
            style = "background: #EE0000; margin-bottom: 10px;"
          ),
          menu_map("qmap", dropdown = drop_map("qmap")),
          select_action("qmap_action"),
          shiny::tableOutput("qmap_summary_latest")
        )
      ),
      main_tabset_panel(
        tab_panel(
          "Map",
          shiny::plotOutput(
            "qmap_heatmap", height = "100%",
            dblclick = "qmap_click",
            brush = brushOpts(id = "qmap_brush", resetOnNew = TRUE)
          )
        ),
        tab_panel(
          "Histogram",
          shiny::plotOutput("qmap_histogram", height = "100%")
        ),
        tab_panel(
          "Summary",
          DT::DTOutput("qmap_summary", height = "100%")
        )
      )
    )
  )
}
