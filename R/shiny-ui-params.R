# © 2019 JAMSTEC
shiny_ui_params <- function() {
  tabPanel(
    "Params for Quantify",
    sidebarLayout(
      sidebar_tabset_panel(
        # Visual
        tab_panel(
          "Menu",
          uiOutput("params_elem_selecter"),
          uiOutput("params_phase_selecter"),
          shiny::checkboxInput("params_origin", "Show origin")
        )
      ),
      main_tabset_panel(
        id = "main_tabset_params",
        tab_panel("Alpha", shiny::plotOutput("params_alpha", height = "100%",)),
        tab_panel("Beta", shiny::plotOutput("params_beta", height = "100%",)),
        tab_panel("Gamma", shiny::plotOutput("params_gamma", height = "100%",)),
        tab_panel("Table", DT::DTOutput("params_qmap", height = "100%",))
      )
    )
  )
}
