shiny_ui_map <- function() {
  shiny::tabPanel(
    "Map",
    shiny::sidebarLayout(
      sidebar_tabset_panel(
        # Menu
        tab_panel(
          "Menu",
          menu_map(
            "xmap", 
            dropdown = drop_map(
              "xmap",
              shiny::checkboxInput("xmap_show_spot", "Show quantified spots")
            )
          ),
          select_action("xmap_action"),
          tags$p(shiny::textOutput("xmap_message_action")),
          shiny::tableOutput("xmap_summary_latest")
        )
      ),
      main_tabset_panel(
        id = "tabset_xmap",
        tab_panel(
          "Map",
          shiny::plotOutput(
            "xmap_heatmap", height = "100%",
            dblclick = "xmap_click",
            brush = brushOpts(id = "xmap_brush", resetOnNew = TRUE)
          )
        ),
        tab_panel(
          "Histogram",
          shiny::plotOutput("xmap_histogram", height = "100%")
        ),
        tab_panel(
          "Summary",
          tags$div(id = "xmap_tab_summary"),
          DT::DTOutput("xmap_summary", height = "100%")
        ),
        tab_panel(
          "Metadata",
          tags$div(id = "xmap_tab_metadata"),
          DT::DTOutput("xmap_meta")
        )
      )
    )
  )
}
