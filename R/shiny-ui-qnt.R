shiny_ui_qnt <- function() {
  tabPanel(
    "Spot",
    sidebarLayout(
      sidebar_tabset_panel(
        # Phase list
        tab_panel(
          "Phase list",
          DTOutput("qnt_phase_list"),
          tags$p("Edit by double click a cell. Save by Ctrl + Enter"),
          actionButton(
            "qnt_phase_list_confirm", label = "Confirm",
            width = "100%", class = "btn-run"
          )
        )
      ),
      main_tabset_panel(
        tab_panel("Analytical conditions", DTOutput("qnt_elm")),
        tab_panel("Spots", DTOutput("qnt_cnd")),
        tab_panel("Wt%", DTOutput("qnt_wt")),
        tab_panel("Net", DTOutput("qnt_net")),
        tab_panel("Peak", DTOutput("qnt_pkint")),
        tab_panel("Background+", DTOutput("qnt_bgp")),
        tab_panel("Background-", DTOutput("qnt_bgm"))
      )
    )
  )
}
