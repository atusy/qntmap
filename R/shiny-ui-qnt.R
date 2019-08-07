shiny_ui_qnt <- function() {
  tabPanel(
    "Spot",
    sidebarLayout(
      sidebar_tabset_panel(
        tab_panel(
          "Menu",
          cssgrid::grid_rowwise(
            tags$p(tags$strong("Plot:"), style = "margin-top: 7px"),
            uiOutput("qnt_ui_x"),
            tags$p("vs.", style = "margin-top: 7px"),
            uiOutput("qnt_ui_y"),
            cols = c("auto 1fr auto 1fr"),
            gap = "5px"
          ),
          tags$hr(),
          tags$p(
            tags$strong("Edit phase list"),
            tippy_info(
              "info-spot-editor",
              "Double click to start edit.<br />Ctrl + Enter to Confirm.",
              placement = "bottom"
            )
          ),
          DTOutput("qnt_phase_list")
        )
      ),
      main_tabset_panel(
        tab_panel(
          "Scatter plot",
          tags$div(
            plotOutput(
              "qnt_plot", height = "100%",
              click = "qnt_click_single",
              dblclick = "qnt_click",
              brush = brushOpts(id = "qnt_brush", resetOnNew = TRUE)
            ),
            style = "height: calc(100% - 2em)"
          ),
          verbatimTextOutput("qnt_plot_id")
        ),
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
