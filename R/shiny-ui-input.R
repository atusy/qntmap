shiny_ui_input <- function(xmap_dir, deadtime, qnt_dir, phase_list) {
  tabPanel(
    "Input",

    # Map
    tags$h1("Mapping"),
    textInput(
      "xmap_dir",
      "Directory containing mapping data.",
      value = xmap_dir
    ),
    numericInput(
      "xmap_deadtime",
      "Dead time in nanoseconds.",
      value = deadtime
    ),
    tags$p("1100 is the default value of JXA-8800"),
    
    # Spot
    tags$h1("Spot analysis"),
    textInput(
      "qnt_dir",
      "Directory containing spot analysis data.",
      value = qnt_dir
    ),
    textInput(
      "phase_list",
      "Path to the csv file identifying phases for each analyzed spots",
      value = phase_list
    ),
    
    # Reload
    actionButton("input_load", label = "Load")
  )
}
