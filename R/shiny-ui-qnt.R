shiny_ui_qnt <- function(qnt_dir) {
  shiny::tabPanel(
    "Spot",
    shiny::sidebarLayout(
      sidebar_tabset_panel(
        # Phase listl
        tab_panel(
          "Phase list",
          DT::DTOutput("qnt_phase_list"),
          tags$p("Edit by double click a cell. Save by Ctrl + Enter"),
          cssgrid::grid_rowwise(
            shiny::actionButton("qnt_phase_list_confirm", label = "Confirm"),
            shiny::actionButton("qnt_phase_list_down", label = "Download"),
            shiny::actionButton("qnt_phase_list_up", label = "Upload"),
            cols = "auto", gap = "2px",
            style = "margin-bottom: 10px;"
          )
        ),
        # Input
        tab_panel(
          "Input",
          shiny::textInput(
            "qnt_dir",
            "Directory containing spot analysis data.",
            value = qnt_dir
          ),
          shiny::actionButton("qnt_read", label = "Reload"),
          tags$p("Check data if data are correctly loaded")
        )
      ),
      main_tabset_panel(
        tab_panel("Analytical conditions", DT::DTOutput("qnt_elm")),
        tab_panel("Spots", DT::DTOutput("qnt_cnd")),
        tab_panel("Wt%", DT::DTOutput("qnt_wt")),
        tab_panel("Net", DT::DTOutput("qnt_net")),
        tab_panel("Peak", DT::DTOutput("qnt_pkint")),
        tab_panel("Background+", DT::DTOutput("qnt_bgp")),
        tab_panel("Background-", DT::DTOutput("qnt_bgm"))
      )
    )
  )
}
