shiny_ui_cluster <- function() {
  shiny::tabPanel(
    "Cluster",
    shiny::sidebarLayout(
      sidebar_tabset_panel(
        # Find outliers
        tab_panel(
          "Menu",
          shiny::actionButton(
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
          shiny::textInput(
            "cluster_suffix", 
            "Suffix of subclusters in Regex",
            value = "_.*",
            width = "100%"
          ),
          select_action("cluster_action"),
          shiny::tableOutput("cluster_summary_latest")
        )
      ),
      main_tabset_panel(
        tab_panel(
          "Plot",
          plotOutput(
            "cluster_heatmap", height = "100%",
            dblclick = "cluster_click",
            brush = brushOpts(id = "cluster_brush", resetOnNew = TRUE)
          )
        ),
        tab_panel(
          "Membership",
          DT::DTOutput("cluster_membership")
        ),
        tab_panel(
          "Centroid",
          DT::DTOutput("cluster_centroid")
        ),
        tab_panel(
          "Summary",
          DT::DTOutput("cluster_summary", height = "100%")
        )
      )
    )
  )
}
