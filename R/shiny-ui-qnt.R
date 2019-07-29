shiny_ui_qnt <- function() {
  shiny::tabPanel(
    "Spot",
    shiny::sidebarLayout(
      sidebar_tabset_panel(
        # Phase listl
        tab_panel(
          "Phase list",
          DT::DTOutput("qnt_phase_list"),
          tags$p("Edit by double click a cell. Save by Ctrl + Enter"),
          shiny::actionButton(
            "qnt_phase_list_confirm", label = "Confirm",
            width = "100%", class = "btn-run"
          )
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
