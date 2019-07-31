shiny_ui_cluster <- function() {
  tabPanel(
    "Cluster",
    sidebarLayout(
      sidebar_tabset_panel(
        # Find outliers
        tab_panel(
          "Menu",
          actionButton(
            "cluster_run", label = "Run cluster analysis", width = "100%",
            class = "btn-run"
          ),
          shinyWidgets::radioGroupButtons(
            inputId = "cluster_subcluster",
            label = "Appearance of subclusters",
            choices = c("Asis", "Integrated"),
            selected = "Asis",
            status = "secondary",
            individual = TRUE
          ),
          textInput(
            "cluster_suffix", 
            "Suffix of subclusters in Regex",
            value = "_.*",
            width = "100%"
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
          "Plot",
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
