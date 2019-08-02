shiny_ui_map <- function() {
  tabPanel(
    "Map",
    sidebarLayout(
      sidebar_tabset_panel(
        # Menu
        tab_panel(
          "Menu",
          menu_map(
            "xmap", 
            dropdown = drop_map(
              "xmap",
              checkboxInput("xmap_show_spot", "Show quantified spots")
            )
          ),
          select_action("xmap_action"),
          tags$strong(
            "Latest summary",
            shiny::actionLink("xmap_tab_summary", "(view full)")
          ),
          tableOutput("xmap_summary_latest")
        )
      ),
      main_tabset_panel(
        id = "main_tabset_xmap",
        tab_panel(
          "Map",
          plotOutput(
            "xmap_heatmap", height = "100%",
            dblclick = "xmap_click",
            brush = brushOpts(id = "xmap_brush", resetOnNew = TRUE)
          )
        ),
        tab_panel(
          "Histogram",
          plotOutput("xmap_histogram", height = "100%")
        ),
        tab_panel("Summary", DTOutput("xmap_summary"))
      )
    )
  )
}
