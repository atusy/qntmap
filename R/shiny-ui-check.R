shiny_ui_check <- function() {
  tabPanel(
    "Check",
    sidebarLayout(
      sidebar_tabset_panel(
        # Find outliers
        tab_panel(
          "Menu",
          tags$h1("Find outliers"),
          uiOutput("outlier_elem_selecter"),
          uiOutput("outlier_phase"),
          shinyWidgets::radioGroupButtons(
            inputId = "outlier_scales",
            label = "Ranges of axes",
            choices = c("Shared", "Individual"),
            selected = "Shared",
            status = "secondary",
            individual = TRUE
          )
        ),
        tab_panel(
          "Detail",
          tags$h1("Multi-phase pixels and outliers"),
          tags$p(
            "To compare data of spot analysis and mapping from the same pixels,",
            "it is important that both are analyzing the same phase.",
            "However, mapping data inevitably contain pixels analyzing",
            "multiple phases because",
            "coordinates of pixels are automatically determined, and",
            "probe diameter is generally larger on mapping than on spot analysis."
          ),
          tags$p(
            "Mapping on multi-phase pixels cause lower or higher intensities of signals from certain elements than spot analysis on corresponding coordinates.",
            "In other words, multi-phase pixels appear as horizontally scattered outliers on the scatter plot on the right pane."
          ),
          
          tags$h1("Algorithm of removing outliers")
        )
      ),
      main_tabset_panel(
        tab_panel(
          "Plot",
          plotOutput("outlier_plot", height = "100%")
        ),
        tab_panel(
          "Initial centroid",
          DTOutput("centroid")
        )
      )
    )
  )
}

ui_check_instruction <- tagList(
  tags$p(
    "The scatter plots on the right pane compares peak X-ray intensities of X-ray maps and spot analysis from the same pixels. Regression curves are drawn by least squares with y-intercept = 0."
  ),
  tags$p(
    "The outliers generally indicate that they are analyzing multiple phases during mapping."
  )
)
