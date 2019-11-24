shiny_ui_cluster <- function() {
  tabPanel(
    "Cluster",
    sidebarLayout(
      sidebar_tabset_panel(
        # Find outliers
        tab_panel(
          "Menu",
          tippy::tippy_this("cluster_run", "It may take a few minutes."),
          cssgrid::grid_rowwise(
            actionButton(
              "cluster_run",
              label = "Run cluster analysis",
              width = "100%",
              class = "btn-run"
            ),
            shinyWidgets::dropdown(
              uiOutput("cluster_elint"),
              circle = FALSE, right = TRUE, width = "200px"
            ),
            cols = ("1fr auto"),
            gap = "3px"
          ),
          cssgrid::grid_layout(
            picker_scale("cluster", label = "Scale", width =  "auto"),
            picker_input(
              inputId = "cluster_subcluster",
              label = "Subclusters",
              choices = c("Separated", "Integrated"),
              selected = "Separated"
            ),
            drop_menu(
              textInput(
                "cluster_suffix", 
                "Suffix of subclusters in Regex",
                value = "_.*",
                width = "100%"
              )
            ),
            cols = "auto 1fr auto",
            column_gap = "3px",
            style = "width: 100%;",
            class = "cssgrid"
          ),
          select_action("cluster_action"),
          tags$strong(
            "Latest summary",
            shiny::actionLink("cluster_tab_summary", "(view full)")
          ),
          tableOutput("cluster_summary_latest")
        )
      ),
      main_tabset_panel(
        id = "main_tabset_cluster",
        tab_panel(
          "Map",
          plotOutput(
            "cluster_heatmap", height = "100%",
            dblclick = "cluster_click",
            brush = brushOpts(id = "cluster_brush", resetOnNew = TRUE)
          )
        ),
        tab_panel("Membership", DTOutput("cluster_membership")),
        tab_panel("Centroid", DTOutput("cluster_centroid")),
        tab_panel("Summary", DTOutput("cluster_summary"))
      )
    )
  )
}
