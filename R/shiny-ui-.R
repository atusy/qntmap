#' @importFrom htmltools tags

sidebar_tabset_panel <- function(...) {
  sidebarPanel(tabsetPanel(...), width = 3, class = "sidebar")
}

main_tabset_panel <- function(...) mainPanel(tabsetPanel(...), width = 9)

tab_panel <- function(title, ...) {
  tabPanel(title, ..., style = "height: calc(100vh - 150px)")
}

css_page <- tags$style(
"
/* Page layout */
.well.sidebar {padding: 0; margin: 0;}
.tab-pane {padding: 10px;}
p {margin-top: 10px;}

/* dropdown */
.cssgrid .sw-dropdown {
  align-self: end;
  margin-bottom: 15px;
}

/* action button */
.action-button { background: #dddddd; }
.action-button.btn-run { 
    background: #00C851; font-weight: bold; margin-bottom: 15px;
  }

/* radioButtons */
.btn.radiobtn.btn-secondary.active { background: #2780e3; color: white; }

/* picker input */
.btn.dropdown-toggle.form-control.shiny-bound-input {border-color: rgb(204, 204, 204);}
"
)

shiny_ui <- function(xmap_dir, qnt_dir, deadtime, phase_list) {
  navbarPage(
    tags$a(
      paste("qntmap", utils::packageVersion("qntmap")),
      href = "https://qntmap.atusy.net/",
      target = "_blank"
    ),
    id = "nav",
    header = css_page,
    selected = "Map",
    shiny_ui_input(
      xmap_dir = xmap_dir, deadtime = deadtime, qnt_dir = qnt_dir, phase_list = phase_list
    ),
    shiny_ui_map(),
    shiny_ui_qnt(),
    shiny_ui_check(),
    shiny_ui_cluster(),
    shiny_ui_quantify()
  )
}

