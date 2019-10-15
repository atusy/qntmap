shiny_ui_quantify <- function() {
  tabPanel(
    "Quantify",
    sidebarLayout(
      sidebar_tabset_panel(
        id = "menu_tabset_qmap",
        # Visual
        tab_panel(
          "Menu",
          cssgrid::grid_rowwise(
            actionButton(
              "qmap_run", label = "Quantify Maps", width = "100%",
              class = "btn-run"
            ),
            shinyWidgets::dropdown(
              shiny::numericInput(
                "qmap_threshold", label = "Threshold of membership degree",
                value = 0.5, min = 0, max = 1, step = 0.05
              ),
              circle = FALSE, right = TRUE, width = "200px"
            ),
            cols = ("1fr auto"),
            gap = "3px"
          ),
          menu_map("qmap", dropdown = drop_map("qmap")),
          select_action("qmap_action"),
          tags$strong(
            "Latest summary",
            shiny::actionLink("qmap_tab_summary", "(view full)")
          ),
          tableOutput("qmap_summary_latest"),
          tags$p(
            "For density correction, go to",
            shiny::actionLink("qmap_tab_adv", "advanced menu.")
          )
        ),
        tab_panel(
          "Adv.",
          DT::DTOutput("qmap_density")
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
        tab_panel("Summary", DTOutput("qmap_summary", height = "100%")),
        tab_panel("Raw", DTOutput("qmap_dt", height = "100%"))
      )
    )
  )
}
