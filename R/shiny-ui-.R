#' @importFrom htmltools tags

sidebar_tabset_panel <- function(...) {
  shiny::sidebarPanel(
    shiny::tabsetPanel(...), width = 3, class = "sidebar"
  )
}

main_tabset_panel <- function(...) {
  shiny::mainPanel(shiny::tabsetPanel(...), width = 9)
}

tab_panel <- function(title, ...) {
  shiny::tabPanel(
    title,
    ...,
    style = "height: calc(100vh - 150px)"
  )
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

/* radioButtons */
.btn.radiobtn.btn-secondary.active {background: #2780e3; color: white;}

/* picker input */
.btn.dropdown-toggle.form-control.shiny-bound-input {border-color: rgb(204, 204, 204);}
"
)

shiny_ui <- function(xmap_dir, qnt_dir, deadtime) {
  shiny::navbarPage(
    paste("qntmap", utils::packageVersion("qntmap")),
    id = "nav",
    header = css_page, selected = "Check",
    shiny_ui_map(xmap_dir = xmap_dir, deadtime = deadtime),
    shiny_ui_qnt(qnt_dir),
    shiny_ui_check(),
    shiny_ui_cluster(),
    shiny_ui_quantify()
  )
}

