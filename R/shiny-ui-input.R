shiny_ui_input <- function(xmap_dir, deadtime, qnt_dir) {
  shiny::tabPanel(
    "Input",
    
    # Map
    tags$h1("Mapping"),
    shiny::textInput(
      "xmap_dir",
      "Directory containing mapping data.",
      value = xmap_dir
    ),
    shiny::numericInput(
      "xmap_deadtime",
      "Dead time in nanoseconds.",
      value = deadtime
    ),
    tags$p("1100 is the default value of JXA-8800"),
    
    # Spot
    tags$h1("Spot analysis"),
    shiny::textInput(
      "qnt_dir",
      "Directory containing spot analysis data.",
      value = qnt_dir
    ),
    
    # Reload
    shiny::actionButton("reload", label = "Reload")
  )
}
