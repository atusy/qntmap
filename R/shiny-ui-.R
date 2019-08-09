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
.sidebar .tab-pane { overflow: auto; }
.tab-pane {padding: 10px;}
h1 {font-size: 2.56rem; font-weight: bold;}
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
a.action-button { background: none; }

/* radioButtons */
.btn.radiobtn.btn-secondary.active { background: #2780e3; color: white; }

/* picker input */
.btn.dropdown-toggle.form-control.shiny-bound-input {border-color: rgb(204, 204, 204);}

/* dt */
.dt-buttons {margin-top: 5px; }

/* tippy.js */
.tippy-tooltip { font-size: 1.6rem; }
.tippy-tooltip a { color: skyblue}
"
)

katex <- tagList(
  tags$link(
    rel = "stylesheet", 
    href = "https://cdn.jsdelivr.net/npm/katex@0.10.1/dist/katex.min.css",
    integrity = "sha384-dbVIfZGuN1Yq7/1Ocstc1lUEm+AT+/rCkibIcC/OmWo5f0EA48Vf8CytHzGrSwbQ", 
    crossorigin = "anonymous"
  ),
  tag(
    "script defer",
    list(
      src = "https://cdn.jsdelivr.net/npm/katex@0.10.1/dist/katex.min.js",
      integrity = "sha384-2BKqo+exmr9su6dir+qCw08N2ZKRucY4PrGQPPWU1A7FtlCGjmEGFqXCv5nyM5Ij",
      crossorigin = "anonymous"
    )
  ),
  tag(
    "script defer",
    list(
      src = "//cdn.jsdelivr.net/npm/katex@0.10.1/dist/contrib/auto-render.min.js",
      integrity = "sha384-kWPLUVMOks5AQFrykwIup5lo0m3iMkkHrD0uJ4H5cjeGihAutqP0yW0J6dpFiVkI",
      crossorigin = "anonymous",
      onload = "renderMathInElement(document.body);"
    )
  ),
  tags$script(
    '
      document.addEventListener("DOMContentLoaded", function(){
        renderMathInElement(document.body, {delimiters: [
          {left: "$$", right: "$$", display: true},
          {left: "$", right: "$", display: false}
        ]});
      })
    '
  )
)

shiny_title <- tags$a(
  paste("qntmap", utils::packageVersion("qntmap")),
  href = "https://qntmap.atusy.net/",
  target = "_blank"
)

shiny_ui <- function(xmap_dir, qnt_dir, deadtime, phase_list, selected = NULL) {
  navbarPage(
    title = shiny_title,
    id = "nav",
    header = list(css_page, tags$title("qntmap")),
    footer = list(katex),
    selected = selected,
    shiny_ui_input(
      xmap_dir = xmap_dir, deadtime = deadtime, qnt_dir = qnt_dir,
      phase_list = phase_list
    ),
    shiny_ui_map(),
    shiny_ui_qnt(),
    shiny_ui_check(),
    shiny_ui_cluster(),
    shiny_ui_quantify()
  )
}

