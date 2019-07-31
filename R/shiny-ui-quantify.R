shiny_ui_quantify <- function() {
  tabPanel(
    "Quantify",
    sidebarLayout(
      sidebar_tabset_panel(
        # Visual
        tab_panel(
          "Menu",
          actionButton(
            "qmap_run", label = "Quantify Maps", width = "100%",
            class = "btn-run"
          ),
          menu_map("qmap", dropdown = drop_map("qmap")),
          select_action("qmap_action"),
          tags$strong(
            "Latest summary",
            shiny::actionLink("qmap_tab_summary", "(view full)")
          ),
          tableOutput("qmap_summary_latest")
        )
      ),
      main_tabset_panel(
        id = "main_tabset_qmap",
        tab_panel(
          "Map",
          plotOutput(
            "qmap_heatmap", height = "100%",
            dblclick = "qmap_click",
            brush = brushOpts(id = "qmap_brush", resetOnNew = TRUE)
          )
        ),
        tab_panel("Histogram", plotOutput("qmap_histogram", height = "100%")),
        tab_panel("Summary", DTOutput("qmap_summary", height = "100%")
        )
      )
    )
  )
}
