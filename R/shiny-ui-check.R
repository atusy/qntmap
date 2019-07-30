shiny_ui_check <- function() {
  tabPanel(
    "Check",
    sidebarLayout(
      sidebar_tabset_panel(
        # Find outliers
        tab_panel(
          "Find outliers",
          ui_check_instruction,
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
