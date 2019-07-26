shiny_ui_map <- function(xmap_dir, deadtime) {
  shiny::tabPanel(
    "Map",
    shiny::sidebarLayout(
      sidebar_tabset_panel(
        # Visual
        tab_panel(
          "Visual",
          menu_map("xmap", dropdown = drop_map("xmap")),
          select_action("xmap_action"),
          tags$p(shiny::textOutput("xmap_message_action")),
          shiny::tableOutput("xmap_summary_latest")
        ),
        # Input
        tab_panel(
          "Input",
          shiny::textInput(
            "xmap_dir",
            "Directory containing mapping data.",
            value = xmap_dir
          ),
          shiny::numericInput(
            "xmap_deadtime",
            "Dead time in nanoseconds.",
            value = deadtime
          ),
          shiny::actionButton(
            "xmap_read", label = "Reload"
          ),
          tags$p(
            "Check metadata if data are correctly loaded"
          )
        )
      ),
      main_tabset_panel(
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
          DT::DTOutput("xmap_summary", height = "100%")
        ),
        tab_panel(
          "Metadata",
          DT::DTOutput("xmap_meta")
        )
      )
    )
  )
}
