shiny_ui_check <- function() {
  shiny::tabPanel(
    "Check",
    shiny::sidebarLayout(
      sidebar_tabset_panel(
        # Find outliers
        tab_panel(
          "Find outliers",
          ui_check_instruction,
          shiny::uiOutput("outlier_elem_selecter"),
          shiny::uiOutput("outlier_phase"),
          shinyWidgets::radioGroupButtons(
            inputId = "outlier_action",
            label = "Appearance of outliers",
            choices = c("Desaturate", "Filter"),
            selected = "Desaturate",
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
          DT::DTOutput("centroid")
        )
      )
    )
  )
}

ui_check_instruction <- htmltools::tagList(
  tags$p(
    "
The scatter plots on the right pane compares peak X-ray intensities of
X-ray maps and spot analysis from the same pixels.
Regression curves are drawn by least squares with y-intercept = 0.
"
  ),
  tags$p(
    "
The outliers generally indicate that
they are analyzing multiple phases during mapping.
"
  )
)
